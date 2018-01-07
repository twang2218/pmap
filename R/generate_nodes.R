#' @title Generate nodes from event logs
#'
#' @param eventlog Event logs
#' @param distinct_customer Whether should only count unique customer
#' @usage generate_nodes(eventlog, distinct_customer = F)
#' @description `eventlog` should be a `data.frame`, which contains, at least, following columns:
#'
#'  * `event_name`: event name. (`character`)
#'  * `event_type`: event type. (`character`)
#'  * `amount`: how many time this event happened in the eventlog
#'
#' `generate_nodes()` will generate the node list from the given `eventlog` for the graph purpose.
#'
#' @return a nodes `data.frame` which represents a event list, it contains `name`, `type` and `amount` columns.
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
  # make 'R CMD check' happy
  event_name <- event_type <- type <- name <- customer_id <- NULL

  if (is.null(eventlog) || is.na(eventlog) || nrow(eventlog) == 0) {
    data.frame(
      name = character(0),
      type = character(0),
      amount = numeric(0)
    )
  } else {
    nodes <- eventlog %>%
      mutate(
        name = as.character(str_trim(event_name)),
        type = as.character(str_trim(event_type))
      )

    if (distinct_customer) {
      nodes <- nodes %>% distinct(name, type, customer_id)
    }

    nodes <- nodes %>%
      group_by(name, type) %>%
      summarize(amount = n()) %>%
      ungroup() %>%
      arrange(name)
  }
}
