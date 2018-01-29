#' @title Generate nodes from event logs
#' @description `eventlog` should be a `data.frame`, which contains, at least, following columns:
#'
#'  * `event_name`: event name. (`character`)
#'  * `event_category`: event category, which is optional. If the `event_category` column is missing, the `event_name` column will be used as the `event_category`. (`character`)
#'  * `amount`: how many time this event happened in the `eventlog`
#'
#' `generate_nodes()` will generate the node list from the given `eventlog` for the graph purpose.
#' @usage generate_nodes(eventlog, distinct_customer = FALSE)
#' @param eventlog Event logs
#' @param distinct_customer Whether should only count unique customer
#' @return a nodes `data.frame` which represents a event list, it contains `name`, `category` and `amount` columns.
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
#' #  1 Event 1 (normal)  normal    958
#' #  2 Event 10 (target) target    948
#' #  3 Event 2 (normal)  normal   1011
#' #  4 Event 3 (normal)  normal   1030
#' #  5 Event 4 (normal)  normal   1072
#' #  6 Event 5 (normal)  normal    968
#' #  7 Event 6 (normal)  normal   1020
#' #  8 Event 7 (normal)  normal    978
#' #  9 Event 8 (normal)  normal   1003
#' # 10 Event 9 (target)  target   1012
#' #
#' # -----------------------------------------------------
#' # Generate nodes and only count by unique customer.
#' # -----------------------------------------------------
#' #
#' nodes <- generate_nodes(eventlog, distinct_customer = TRUE)
#' nodes
#' # # A tibble: 10 x 3
#' #    name              category   amount
#' #    <chr>             <chr>   <int>
#' #  1 Event 1 (normal)  normal    100
#' #  2 Event 10 (target) target    100
#' #  3 Event 2 (normal)  normal    100
#' #  4 Event 3 (normal)  normal    100
#' #  5 Event 4 (normal)  normal    100
#' #  6 Event 5 (normal)  normal    100
#' #  7 Event 6 (normal)  normal    100
#' #  8 Event 7 (normal)  normal    100
#' #  9 Event 8 (normal)  normal    100
#' # 10 Event 9 (target)  target    100
#' @importFrom dplyr        %>%
#' @importFrom dplyr        distinct
#' @importFrom dplyr        mutate
#' @importFrom dplyr        group_by
#' @importFrom dplyr        summarize
#' @importFrom data.table   setorder
#' @importFrom stringr      str_trim
#' @export
generate_nodes <- function(eventlog, distinct_customer = FALSE) {
  # make 'R CMD check' happy
  event_name <- event_category <- category <- name <- customer_id <- NULL

  if (is.null(eventlog) || is.na(eventlog) || nrow(eventlog) == 0) {
    data.frame(
      name = character(0),
      category = character(0),
      amount = numeric(0)
    )
  } else {
    nodes <- eventlog %>% dplyr::mutate(name = stringr::str_trim(as.character(event_name)))

    if ("event_category" %in% colnames(eventlog)) {
      nodes <- nodes %>% dplyr::mutate(category = stringr::str_trim(as.character(event_category)))
    } else {
      # if `event_category` column is not provided, then use the `event_name`
      # column as the `category`.
      nodes <- nodes %>% dplyr::mutate(category = name)
    }

    if (distinct_customer) {
      nodes <- nodes %>% dplyr::distinct(name, category, customer_id)
    }

    nodes <- nodes %>%
      dplyr::group_by(name, category) %>%
      dplyr::summarize(amount = n()) %>%
      dplyr::ungroup() %>%
      data.table::setorder(name)
  }
}
