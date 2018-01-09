#' @title Generate edges from event logs
#' @usage generate_edges(eventlog, distinct_customer = FALSE, target_types = NULL)
#' @param eventlog Event logs
#' @param distinct_customer Whether should only count unique customer
#' @param target_types A vector contains the target event types. By default, it's `NULL`, which means every paths count. If it's contains the target event type, then only paths reaches the target event count.
#' @return a `data.frame` of edges with `from`, `to` and `amount` columns.
#' @description `eventlog` should be a `data.frame` or `data.table`, which contains, at least, following columns:
#'
#'  * `timestamp`: timestamp column which indicates when event happened. (`POSIXct`)
#'  * `customer_id`: customer identifier. (`character`)
#'  * `event_name`: event name. (`character`)
#'  * `event_type`: event type. (`character`)
#' @examples
#' # -----------------------------------------------------
#' # Generating edges and count every paths no matter whether
#' # it's from the same customer or not.
#' # -----------------------------------------------------
#' eventlog <- generate_eventlog()
#' edges <- generate_edges(eventlog)
#' head(edges)
#' # # A tibble: 6 x 3
#' #   from             to                amount
#' #   <chr>            <chr>              <int>
#' # 1 Event 1 (normal) Event 1 (normal)      13
#' # 2 Event 1 (normal) Event 10 (target)      3
#' # 3 Event 1 (normal) Event 2 (normal)       7
#' # 4 Event 1 (normal) Event 3 (normal)       9
#' # 5 Event 1 (normal) Event 4 (normal)      11
#' # 6 Event 1 (normal) Event 5 (normal)      14
#' str(edges)
#' # Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	100 obs. of  3 variables:
#' #  $ from  : chr  "Event 1 (normal)" "Event 1 (normal)" "Event 1 (normal)" "Event 1 (normal)" ...
#' #  $ to    : chr  "Event 1 (normal)" "Event 10 (target)" "Event 2 (normal)" "Event 3 (normal)" ...
#' #  $ amount: int  13 3 7 9 11 14 8 12 16 15 ...
#' #  - attr(*, ".internal.selfref")=<externalptr> 
#' # -----------------------------------------------------
#' # Generate edges by specify the target types, and the paths 
#' # not reaching the target type events will be ignored.
#' # -----------------------------------------------------
#' edges <- generate_edges(eventlog, target_types = c("target"))
#' str(edges)
#' # Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	80 obs. of  3 variables:
#' #  $ from  : chr  "Event 1 (normal)" "Event 1 (normal)" "Event 1 (normal)" "Event 1 (normal)" ...
#' #  $ to    : chr  "Event 1 (normal)" "Event 10 (target)" "Event 2 (normal)" "Event 3 (normal)" ...
#' #  $ amount: int  12 3 7 7 11 9 7 8 16 15 ...
#' #  - attr(*, ".internal.selfref")=<externalptr> 
#' @importFrom dplyr        %>%
#' @importFrom dplyr        arrange
#' @importFrom dplyr        distinct
#' @importFrom dplyr        group_by
#' @importFrom dplyr        summarize
#' @importFrom dplyr        ungroup
#' @importFrom dplyr        mutate_if
#' @importFrom dplyr        n
#' @importFrom data.table   data.table
#' @importFrom data.table   rbindlist
#' @export
generate_edges <- function(eventlog, distinct_customer = FALSE, target_types = NULL) {
  # make 'R CMD check' happy
  customer_id <- timestamp <- from <- to <- NULL

  # sort by customer_id and timestamp
  eventlog <- data.table(eventlog) %>% arrange(customer_id, timestamp)

  empty_edges <- data.frame(
    from = character(0),
    to = character(0),
    amount = numeric(0)
  )

  # Return empty edges if given eventlog is empty
  if (nrow(eventlog) == 0) {
    return(empty_edges)
  }

  edges <- list()
  temp_edges <- list()
  previous <- NULL

  # function to append edges
  append_edges <- function(edges, new_edges) {
    if (is.null(edges)) {
      edges <- list()
    }
    if (length(new_edges) > 0) {
      for (i in 1:length(new_edges)) {
        edges[[length(edges) + 1]] <- new_edges[[i]]
      }
    }
    return(edges)
  }

  # function to assign previous based on 'target_types' value
  assign_previous <- function(current, target_types) {
    # target events should not be the starting point
    if (current["event_type"] %in% target_types) {
      return(NULL)
    } else {
      return(current)
    }
  }

  # function to check whether `target_types` is empty
  is_target_types_empty <- function(target_types) {
    return(is.null(target_types) || length(target_types) == 0)
  }

  # `apply()` is about up to 10x faster than `for-loop`
  apply(eventlog, 1, function(current) {
    # `previous` is not empty
    if (!any(is.null(previous))) {
      # pre-process
      # Reached another customer
      if (current["customer_id"] != previous["customer_id"]) {
        if (is_target_types_empty(target_types)) {
          # Not have target_types, so every path count.
          edges <<- append_edges(edges, temp_edges)
        }

        # it's a new customer, so set it to 'previous' and continue to the next record, if it's not 'target_types'
        previous <<- assign_previous(current, target_types)

        # clear temp_edges
        temp_edges <<- list()
        return()
      }

      edge <- list(
        from = previous["event_name"],
        to = current["event_name"],
        customer_id = current["customer_id"]#,
        # duration = current["timestamp"] - previous["timestamp"]
      )
      temp_edges[[length(temp_edges) + 1]] <<- edge
    }

    # post-process
    previous <<- assign_previous(current, target_types)
    if (current["event_type"] %in% target_types) {
      # Has target_types, so, only paths reaches the 'target_types' count.
      # put temp_edges to final edges set as it reached the 'target_types'
      edges <<- append_edges(edges, temp_edges)
      # clear `temp_edges` as we reaches the target
      temp_edges <<- list()
    }
  })

  if (is_target_types_empty(target_types)) {
    # Some edges remains in the 'temp_edges' and we don't care about the 'target_types', so every path count.
    edges <- append_edges(edges, temp_edges)
  }

  # Cannot find any edges, so return an empty data frame.
  if (length(edges) == 0) {
    return(empty_edges)
  }

  edges <- rbindlist(edges)

  if (distinct_customer) {
    edges <- distinct(edges, from, to, customer_id)
  }

  edges <- edges %>%
    group_by(from, to) %>%
    # Add attributes: `amount` => count
    summarize(amount = n()) %>%
    ungroup() %>%
    mutate_if(is.factor, as.character) %>%
    arrange(from, to)

  return(edges)
}
