#' @title Render the process map
#' @usage render_pmap(p, title = NULL)
#' @param p the process map object created by `create_pmap()` function
#' @param title The title of rendered graph
#' @description Basically, this function just called `DiagrammeR::render_graph()`
#' @importFrom DiagrammeR   render_graph
#' @export
render_pmap <- function(p, title = NULL) {
  render_graph(p, title = title)
}
