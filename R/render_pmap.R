#' @title Render the process map
#' @description Basically, this function just called `DiagrammeR::render_graph()`
#' @usage render_pmap(p, title = NULL)
#' @param p the process map object created by `create_pmap_graph()` function
#' @param title The title of rendered graph
#' @examples
#' library(dplyr)
#' p <- generate_eventlog() %>% create_pmap(target_types = c("target"))
#' render_pmap
#' @seealso [create_pmap]
#' @importFrom DiagrammeR   %>%
#' @importFrom DiagrammeR   render_graph
#' @importFrom DiagrammeR   select_nodes_by_degree
#' @importFrom DiagrammeR   get_selection
#' @importFrom DiagrammeR   delete_nodes_ws
#' @export
render_pmap <- function(p, title = NULL) {
  # remove node without edges
  zero_degree_nodes <- DiagrammeR::select_nodes_by_degree(p, "deg == 0")
  if (!any(is.na(DiagrammeR::get_selection(zero_degree_nodes)))) {
    p <- DiagrammeR::delete_nodes_ws(zero_degree_nodes)
  }

  DiagrammeR::render_graph(p, title = title)
}
