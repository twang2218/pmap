#' @title Generate edges from event logs
#' @usage generate_edges(eventlog, distinct_customer = FALSE, target_categories = NULL)
#' @param eventlog Event logs
#' @param distinct_customer Whether should only count unique customer
#' @param target_categories A vector contains the target event categories. By default, it's `NULL`, which means every paths count. If it's contains the target event category, then only paths reaches the target event count.
#' @return a `data.frame` of edges with `from`, `to` and `amount` columns.
#' @description `eventlog` should be a `data.frame` or `data.table`, which contains, at least, following columns:
#'
#'  * `timestamp`: timestamp column which indicates when event happened. The column's data type should be `POSIXct`, otherwise it will be converted to `POSIXct` automatically. (`POSIXct`)
#'  * `customer_id`: customer identifier. (`character`)
#'  * `event_name`: event name. (`character`)
#'  * `event_category`: event category. (`character`)
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
#' # Generate edges by specify the target categories, and the paths
#' # not reaching the target category events will be ignored.
#' # -----------------------------------------------------
#' edges <- generate_edges(eventlog, target_categories = c("target"))
#' str(edges)
#' # Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	80 obs. of  3 variables:
#' #  $ from  : chr  "Event 1 (normal)" "Event 1 (normal)" "Event 1 (normal)" "Event 1 (normal)" ...
#' #  $ to    : chr  "Event 1 (normal)" "Event 10 (target)" "Event 2 (normal)" "Event 3 (normal)" ...
#' #  $ amount: int  12 3 7 7 11 9 7 8 16 15 ...
#' #  - attr(*, ".internal.selfref")=<externalptr>
#' @importFrom dplyr        %>%
#' @importFrom dplyr        distinct
#' @importFrom dplyr        group_by
#' @importFrom dplyr        summarize
#' @importFrom dplyr        ungroup
#' @importFrom dplyr        mutate_if
#' @importFrom dplyr        mutate
#' @importFrom dplyr        left_join
#' @importFrom dplyr        inner_join
#' @importFrom dplyr        select
#' @importFrom dplyr        filter
#' @importFrom dplyr        n
#' @importFrom data.table   setorder
#' @importFrom data.table   as.data.table
#' @importFrom stats        median
#' @importFrom stringr      str_trim
#' @export
generate_edges <- function(eventlog, distinct_customer = FALSE, target_categories = NULL) {
  # return empty edge if eventlog is empty
  if (nrow(eventlog) == 0) {
    return(
      data.frame(
        from = character(0),
        to = character(0),
        amount = numeric(0)
      )
    )
  }

  # make 'R CMD check' happy
  event_name <- event_category <- is_target <- customer_id <- timestamp <- last_target_date <-
  from <- from_cid <- from_time <- from_is_target <-
  to_cid <- to <-
  duration <- mean_duration <- median_duration <- max_duration <- min_duration <- NULL

  # make sure there is no factor in the `eventlog` and trim the SPACE
  eventlog <- eventlog %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate_if(is.character, stringr::str_trim)

  target_categories <- stringr::str_trim(target_categories)

  # Attach `is_target` column
  if (length(target_categories) > 0) {
    categories <- eventlog %>%
      dplyr::distinct(event_category) %>%
      dplyr::left_join(
        data.frame(
          event_category = target_categories,
          is_target = TRUE,
          stringsAsFactors = FALSE
        ),
        by = "event_category"
      ) %>%
      dplyr::select(event_category, is_target) %>%
      dplyr::mutate(is_target = !is.na(is_target))

    eventlog <- dplyr::inner_join(eventlog, categories, by = "event_category")
  } else {
    eventlog <- dplyr::mutate(eventlog, is_target = FALSE)
  }

  # convert timestamp if it's not POSIXct yet.
  if (!inherits(eventlog$timestamp, "POSIXct")) {
    eventlog <- dplyr::mutate(eventlog, timestamp = as.POSIXct(timestamp))
  }

  # Construct potential edges
  eventlog <- data.table::as.data.table(eventlog) %>% data.table::setorder(customer_id, timestamp)

  size <- nrow(eventlog)
  begin <- eventlog[-size, ]
  end <- eventlog[-1, ]

  edges <- data.frame(
      from_time = begin$timestamp,
      from = begin$event_name,
      customer_id = begin$customer_id,
      from_is_target = begin$is_target,
      to_time = end$timestamp,
      to = end$event_name,
      to_cid = end$customer_id,
      to_is_target = end$is_target,
      duration = end$time - begin$time,
      stringsAsFactors = FALSE
    ) %>%
    dplyr::filter(customer_id == to_cid & !from_is_target)


  # prune edges by target_categories
  if (length(target_categories) > 0) {
    # find the last target event date
    customer_last_target_date <- eventlog %>%
      dplyr::filter(is_target) %>%
      dplyr::group_by(customer_id) %>%
      dplyr::summarize(last_target_date = max(timestamp))

    # prune all the edges with event after the last target event date
    edges <- edges %>%
      dplyr::inner_join(customer_last_target_date, by = "customer_id") %>%
      dplyr::filter(from_time < last_target_date)
  }

  # Only count customer once if `distinct_customer` flag is set
  if (distinct_customer) {
    edges <- edges %>%
      dplyr::group_by(from, to, customer_id) %>%
      dplyr::summarize(
        mean_duration = mean(duration),
        median_duration = stats::median(duration),
        max_duration = max(duration),
        min_duration = min(duration)
      )
  } else {
    edges <- dplyr::mutate(
      edges,
      mean_duration = duration,
      median_duration = duration,
      max_duration = duration,
      min_duration = duration,
    )
  }

  edges <- edges %>%
    dplyr::group_by(from, to) %>%
    dplyr::summarize(
      amount = n(),
      mean_duration = format_duration(mean(mean_duration)),
      median_duration = format_duration(stats::median(median_duration)),
      max_duration = format_duration(max(max_duration)),
      min_duration = format_duration(min(min_duration))
    ) %>%
    dplyr::ungroup() %>%
    data.table::setorder(from, to)

  return(edges)
}
