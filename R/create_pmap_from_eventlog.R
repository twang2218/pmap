#' @title Create the process map from event log directly
#' @usage create_pmap_from_eventlog(eventlog)
#' @param eventlog Event log
#' @description Create the process map by analysing given `eventlog` and extract the nodes by `generate_nodes()` and edges by `generate_edges()`.
#' @export
create_pmap_from_eventlog <- function(eventlog) {
  nodes <- generate_nodes(eventlog)
  edges <- generate_edges(eventlog)
  p <- create_pmap(nodes, edges)
  return(p)
}
