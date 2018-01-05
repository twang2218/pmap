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
#' @usage generate_nodes(eventlog)
#' @description `eventlog` should be a `data.frame`, which contains, at least, following columns:
#'
#'  * `event_name`: event name. (`character`)
#'  * `is_target`: whether it's the final stage. (`logical`)
#'
#' `generate_nodes()` will generate the node list from the given `eventlog` for the graph purpose.
#'
#' @return a nodes `data.frame` which represents a event list, it contains `name`, `is_target` columns.
#' @importFrom dplyr      %>%
#' @importFrom dplyr      select
#' @importFrom dplyr      distinct
#' @importFrom dplyr      rename
#' @importFrom dplyr      mutate
#' @importFrom dplyr      arrange
#' @importFrom stringr    str_trim
#' @export
generate_nodes <- function(eventlog) {
  if (is.null(eventlog) || is.na(eventlog) || nrow(eventlog) == 0) {
    data.frame(
      event_name = character(0),
      is_target = logical(0)
    )
  } else {
    eventlog %>%
      select(event_name, is_target) %>%
      mutate(event_name = str_trim(event_name)) %>%
      distinct() %>%
      rename(name = event_name) %>%
      mutate(name = as.character(name)) %>%
      arrange(name)
  }
}
