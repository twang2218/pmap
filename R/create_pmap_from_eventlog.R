#' @title Create the process map from event log directly
#' @usage create_pmap_from_eventlog(eventlog, distinct_customer = F, target_types = NULL)
#' @param eventlog Event log
#' @param distinct_customer Whether should count distinct customer only. Default is `FALSE`.
#' @param target_types A vector contains the target event types
#' @description Create the process map by analysing given `eventlog` and extract the nodes by `generate_nodes()` and edges by `generate_edges()`.
#' @export
create_pmap_from_eventlog <- function(eventlog, distinct_customer = F, target_types = NULL) {
  nodes <- generate_nodes(eventlog, distinct_customer)
  edges <- generate_edges(eventlog, distinct_customer, target_types)
  p <- create_pmap(nodes, edges, target_types)
  return(p)
}
