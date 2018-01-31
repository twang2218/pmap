#' @title Generate nodes from event logs
#' @description `eventlog` should be a `data.frame`, which contains, at least, following columns:
#'
#'  * `activity`: activity name. (`character`)
#'  * `category`: activity category, which is optional. If the `category` column is missing, the `activity` column will be used as the `category`. (`character`)
#'  * `amount`: how many time this activity happened in the `eventlog`
#'
#' `generate_nodes()` will generate the node list from the given `eventlog` for the graph purpose.
#' @usage generate_nodes(eventlog, distinct_case = FALSE)
#' @param eventlog Event logs
#' @param distinct_case Whether should only count unique case
#' @return a nodes `data.frame` which represents a event log, it contains `name`, `category` and `amount` columns.
#' @examples
#' # -----------------------------------------------------
#' # Generate nodes from eventlog and count every event
#' # -----------------------------------------------------
#' eventlog <- generate_eventlog(10000, 100)
#' nodes <- generate_nodes(eventlog)
#' print(nodes)
#' # # A tibble: 10 x 3
#' #    name              category   amount
#' #    <chr>             <chr>   <int>
#' #  1 Activity 1 (normal)  normal    958
#' #  2 Activity 10 (target) target    948
#' #  3 Activity 2 (normal)  normal   1011
#' #  4 Activity 3 (normal)  normal   1030
#' #  5 Activity 4 (normal)  normal   1072
#' #  6 Activity 5 (normal)  normal    968
#' #  7 Activity 6 (normal)  normal   1020
#' #  8 Activity 7 (normal)  normal    978
#' #  9 Activity 8 (normal)  normal   1003
#' # 10 Activity 9 (target)  target   1012
#' #
#' # -----------------------------------------------------
#' # Generate nodes and only count by unique case.
#' # -----------------------------------------------------
#' #
#' nodes <- generate_nodes(eventlog, distinct_case = TRUE)
#' nodes
#' # # A tibble: 10 x 3
#' #    name              category   amount
#' #    <chr>             <chr>   <int>
#' #  1 Activity 1 (normal)  normal    100
#' #  2 Activity 10 (target) target    100
#' #  3 Activity 2 (normal)  normal    100
#' #  4 Activity 3 (normal)  normal    100
#' #  5 Activity 4 (normal)  normal    100
#' #  6 Activity 5 (normal)  normal    100
#' #  7 Activity 6 (normal)  normal    100
#' #  8 Activity 7 (normal)  normal    100
#' #  9 Activity 8 (normal)  normal    100
#' # 10 Activity 9 (target)  target    100
#' @importFrom dplyr        %>%
#' @importFrom dplyr        distinct
#' @importFrom dplyr        mutate
#' @importFrom dplyr        group_by
#' @importFrom dplyr        summarize
#' @importFrom data.table   setorder
#' @importFrom stringr      str_trim
#' @export
generate_nodes <- function(eventlog, distinct_case = FALSE) {
  # make 'R CMD check' happy
  activity <- category <- category <- name <- case_id <- NULL

  if (is.null(eventlog) || is.na(eventlog) || nrow(eventlog) == 0) {
    data.frame(
      name = character(0),
      category = character(0),
      amount = numeric(0)
    )
  } else {
    nodes <- eventlog %>% dplyr::mutate(name = stringr::str_trim(as.character(activity)))

    if ("category" %in% colnames(eventlog)) {
      nodes <- nodes %>% dplyr::mutate(category = stringr::str_trim(as.character(category)))
    } else {
      # if `category` column is not provided, then use the `activity`
      # column as the `category`.
      nodes <- nodes %>% dplyr::mutate(category = name)
    }

    if (distinct_case) {
      nodes <- nodes %>% dplyr::distinct(name, category, case_id)
    }

    nodes <- nodes %>%
      dplyr::group_by(name, category) %>%
      dplyr::summarize(amount = n()) %>%
      dplyr::ungroup() %>%
      data.table::setorder(name)
  }
}
