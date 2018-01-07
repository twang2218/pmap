# make 'R CMD check' happy
utils::globalVariables(c(
  # generate_nodes()
  "event_name",
  "is_target",
  "name"
))

#' @title Generate nodes from event logs
#'
#' @param eventlog Event logs
#' @param distinct_customer Whether should only count unique customer
#' @usage generate_nodes(eventlog, distinct_customer = F)
#' @description `eventlog` should be a `data.frame`, which contains, at least, following columns:
#'
#'  * `event_name`: event name. (`character`)
#'  * `is_target`: whether it's the final stage. (`logical`)
#'  * `amount`: how many time this event happened in the eventlog
#'
#' `generate_nodes()` will generate the node list from the given `eventlog` for the graph purpose.
#'
#' @return a nodes `data.frame` which represents a event list, it contains `name`, `is_target` and `amount` columns.
#' @importFrom dplyr      %>%
#' @importFrom dplyr      select
#' @importFrom dplyr      distinct
#' @importFrom dplyr      rename
#' @importFrom dplyr      mutate
#' @importFrom dplyr      arrange
#' @importFrom dplyr      group_by
#' @importFrom dplyr      summarize
#' @importFrom stringr    str_trim
#' @export
generate_nodes <- function(eventlog, distinct_customer = F) {
  if (is.null(eventlog) || is.na(eventlog) || nrow(eventlog) == 0) {
    data.frame(
      event_name = character(0),
      is_target = logical(0),
      amount = numeric(0)
    )
  } else {
    nodes <- eventlog %>%
      mutate(name = as.character(str_trim(event_name)))

    if (distinct_customer) {
      nodes <- nodes %>% distinct(name, is_target, customer_id)
    }

    nodes <- nodes %>%
      group_by(name, is_target) %>%
      summarize(amount = n()) %>%
      ungroup() %>%
      arrange(name)
  }
}
