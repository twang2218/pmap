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
  zero_degree_nodes <- p %>% select_nodes_by_degree("deg == 0") %>% get_selection()
  if (!is.na(zero_degree_nodes) && length(zero_degree_nodes) > 0) {
    p <- p %>% select_nodes_by_degree("deg == 0") %>% delete_nodes_ws()
  }

  render_graph(p, title = title)
}
