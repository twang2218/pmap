#' @title Generate edges from event logs
#' @usage generate_edges(eventlog, distinct_case = FALSE, target_categories = NULL)
#' @param eventlog Event logs
#' @param distinct_case Whether should only count unique case
#' @param target_categories A vector contains the target activity categories. By default, it's `NULL`, which means every paths count. If it's contains the target activity category, then only paths reaches the target activity count.
#' @return a `data.frame` of edges with `from`, `to` and `amount` columns.
#' @description `eventlog` should be a `data.frame` or `data.table`, which contains, at least, following columns:
#'
#'  * `timestamp`: timestamp column which indicates when activity happened. The column's data type should be `POSIXct`, otherwise it will be converted to `POSIXct` automatically. (`POSIXct`)
#'  * `case_id`: case identifier. (`character`)
#'  * `activity`: activity name. (`character`)
#'  * `category`: activity category. (`character`)
#' @examples
#' # -----------------------------------------------------
#' # Generating edges and count every paths no matter whether
#' # it's from the same case or not.
#' # -----------------------------------------------------
#' eventlog <- generate_eventlog()
#' edges <- generate_edges(eventlog)
#' # Have a look on generated edges
#' head(edges)
#' # # A tibble: 6 x 7
#' #   from    to     amount mean_duration median_duration max_duration min_duration
#' #   <chr>   <chr>   <int> <chr>         <chr>           <chr>        <chr>
#' # 1 Activi… Activ…     12 1.02 weeks    1.12 weeks      1.91 weeks   2 hours
#' # 2 Activi… Activ…      5 3.59 days     4.41 days       1.03 weeks   17.33 hours
#' # 3 Activi… Activ…     10 1.13 weeks    7 days          2.93 weeks   1.51 days
#' # 4 Activi… Activ…      1 2.46 weeks    2.46 weeks      2.46 weeks   2.46 weeks
#' # 5 Activi… Activ…      3 1.72 weeks    1.2 weeks       2.79 weeks   1.16 weeks
#' # 6 Activi… Activ…      8 3.38 days     2.2 days        1.85 weeks   15.1 hours
#'
#' str(edges)
#' # Classes ‘tbl_df’, ‘tbl’ and 'data.frame':       161 obs. of  7 variables:
#' #  $ from           : chr  "Activity 1 (normal)" "Activity 1 (normal)" "Activity 1 (normal)"
#' # "Activity 1 (normal)" ...
#' #  $ to             : chr  "Activity 1 (normal)" "Activity 10 (phone)" "Activity 11 (phone)"
#' # "Activity 12 (phone)" ...
#' #  $ amount         : int  12 5 10 1 3 8 2 15 9 15 ...
#' #  $ mean_duration  : chr  "1.02 weeks" "3.59 days" "1.13 weeks" "2.46 weeks" ...
#' #  $ median_duration: chr  "1.12 weeks" "4.41 days" "7 days" "2.46 weeks" ...
#' #  $ max_duration   : chr  "1.91 weeks" "1.03 weeks" "2.93 weeks" "2.46 weeks" ...
#' #  $ min_duration   : chr  "2 hours" "17.33 hours" "1.51 days" "2.46 weeks" ...
#' #
#' # -----------------------------------------------------
#' # Generate edges by specify the target categories, and the paths
#' # not reaching the target category activities will be ignored.
#' # -----------------------------------------------------
#' edges <- generate_edges(eventlog, target_categories = c("target"))
#' str(edges)
#' # Classes ‘tbl_df’, ‘tbl’ and 'data.frame':       115 obs. of  7 variables:
#' #  $ from           : chr  "Activity 1 (normal)" "Activity 1 (normal)" "Activity 1 (normal)"
#' # "Activity 1 (normal)" ...
#' #  $ to             : chr  "Activity 1 (normal)" "Activity 11 (phone)" "Activity 13 (target)"
#' # "Activity 14 (target)" ...
#' #  $ amount         : int  1 3 1 4 2 6 1 1 3 1 ...
#' #  $ mean_duration  : chr  "4.4 days" "2.12 weeks" "2.89 hours" "1.47 weeks" ...
#' #  $ median_duration: chr  "4.4 days" "2.15 weeks" "2.89 hours" "1.04 weeks" ...
#' #  $ max_duration   : chr  "4.4 days" "2.16 weeks" "2.89 hours" "3.53 weeks" ...
#' #  $ min_duration   : chr  "4.4 days" "2.03 weeks" "2.89 hours" "1.76 days" ...
#' #
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
generate_edges <- function(eventlog, distinct_case = FALSE, target_categories = NULL) {
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
  activity <- category <- is_target <- case_id <- timestamp <- last_target_date <-
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
      dplyr::distinct(category) %>%
      dplyr::left_join(
        data.frame(
          category = target_categories,
          is_target = TRUE,
          stringsAsFactors = FALSE
        ),
        by = "category"
      ) %>%
      dplyr::select(category, is_target) %>%
      dplyr::mutate(is_target = !is.na(is_target))

    eventlog <- dplyr::inner_join(eventlog, categories, by = "category")
  } else {
    eventlog <- dplyr::mutate(eventlog, is_target = FALSE)
  }

  # convert timestamp if it's not POSIXct yet.
  if (!inherits(eventlog$timestamp, "POSIXct")) {
    eventlog <- dplyr::mutate(eventlog, timestamp = as.POSIXct(timestamp))
  }

  # Construct potential edges
  eventlog <- data.table::as.data.table(eventlog) %>% data.table::setorder(case_id, timestamp)

  size <- nrow(eventlog)
  begin <- eventlog[-size, ]
  end <- eventlog[-1, ]

  edges <- data.frame(
      from_time = begin$timestamp,
      from = begin$activity,
      case_id = begin$case_id,
      from_is_target = begin$is_target,
      to_time = end$timestamp,
      to = end$activity,
      to_cid = end$case_id,
      to_is_target = end$is_target,
      duration = end$time - begin$time,
      stringsAsFactors = FALSE
    ) %>%
    dplyr::filter(case_id == to_cid & !from_is_target)


  # prune edges by target_categories
  if (length(target_categories) > 0) {
    # find the last target activity date
    case_last_target_date <- eventlog %>%
      dplyr::filter(is_target) %>%
      dplyr::group_by(case_id) %>%
      dplyr::summarize(last_target_date = max(timestamp))

    # prune all the edges with activity after the last target activity date
    edges <- edges %>%
      dplyr::inner_join(case_last_target_date, by = "case_id") %>%
      dplyr::filter(from_time < last_target_date)
  }

  # Only count case once if `distinct_case` flag is set
  if (distinct_case) {
    edges <- edges %>%
      dplyr::group_by(from, to, case_id) %>%
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
