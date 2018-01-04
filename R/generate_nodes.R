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
#' @description `eventlog` should be a `data.frame` or `data.table`, which contains, at least, following columns:
#'
#'  * `event_name`: event name. (`character`)
#'  * `is_target`: whether it's the final stage. (`logical`)
#'
#' `generate_nodes()` will generate the node list from the given `eventlog` for the graph purpose.
#'
#' @return a `data.frame` of nodes
#' @importFrom dplyr      %>%
#' @importFrom dplyr      select
#' @importFrom dplyr      distinct
#' @importFrom dplyr      rename
#' @importFrom dplyr      mutate
#' @importFrom dplyr      arrange
#' @export
generate_nodes <- function(eventlog) {
  eventlog %>%
    select(event_name, is_target) %>%
    distinct() %>%
    rename(name = event_name) %>%
    mutate(name = as.character(name), percentage = 0.5) %>%
    arrange(name)
}
