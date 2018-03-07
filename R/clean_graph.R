#' @importFrom DiagrammeR   select_nodes_by_degree
#' @importFrom DiagrammeR   get_selection
#' @importFrom DiagrammeR   delete_nodes_ws
#' @importFrom DiagrammeR   count_nodes
#' @importFrom DiagrammeR   clear_selection
clean_graph <- function(p) {
  if (DiagrammeR::count_nodes(p) > 0) {
    # remove node without edges
    zero_degree_nodes <- DiagrammeR::select_nodes_by_degree(p, "deg == 0")
    if (!any(is.na(DiagrammeR::get_selection(zero_degree_nodes)))) {
      p <- DiagrammeR::delete_nodes_ws(zero_degree_nodes) %>% clear_selection()
    }
  }
  return(p)
}
