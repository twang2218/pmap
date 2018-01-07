#' @title Generate edges from event logs
#' @usage generate_edges(eventlog, distinct_customer = F, target_types = NULL)
#' @param eventlog Event logs
#' @param distinct_customer Whether should only count unique customer
#' @param target_types A vector contains the target event types
#' @description `eventlog` should be a `data.frame` or `data.table`, which contains, at least, following columns:
#'
#'  * `timestamp`: timestamp column which indicates when event happened. (`POSIXct`)
#'  * `customer_id`: cutomer identifier. (`character`)
#'  * `event_name`: event name. (`character`)
#'  * `event_type`: event type. (`character`)
#' @return a `data.frame` of edges with `from`, `to` and `amount` columns.
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
generate_edges <- function(eventlog, distinct_customer = F, target_types = NULL) {
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
  previous <- NA

  # `apply()` is about up to 10x faster than `for-loop`
  apply(eventlog, 1, function(current) {
    if (!any(is.na(previous))) {
      # pre-process
      if (current["customer_id"] != previous["customer_id"]) {
        # it's a new customer, so set it to 'previous' and continue to the next record
        previous <<- current
        # clear temp_edges
        temp_edges <<- list()
        return
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
    if (current["event_type"] %in% target_types) {
      # put temp_edges to final edges set
      if (length(temp_edges) > 0) {
        for (i in 1:length(temp_edges)) {
          edges[[length(edges) + 1]] <<- temp_edges[[i]]
        }
      }
      # target events should not be the starting point
      previous <<- NA
      # clear temp_edges
      temp_edges <<- list()
    } else {
      previous <<- current
    }
  })

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
