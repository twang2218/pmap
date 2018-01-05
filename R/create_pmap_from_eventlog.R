#' @title Create the process map from event log directly
#' @usage create_pmap_from_eventlog(eventlog, render = T)
#' @param eventlog Event log
#' @param render Whether should call `render_graph()` on the final result. Set to `FALSE` if you want further manipulate the result by `DiagrammeR` functions.
#' @description Create the process map by analysing given `eventlog` and extract the nodes by `generate_nodes()` and edges by `generate_edges()`.
#' @export
create_pmap_from_eventlog <- function(eventlog, render = T) {
  nodes <- generate_nodes(eventlog)
  edges <- generate_edges(eventlog)
  p <- create_pmap(nodes, edges, render)
  return(p)
}
