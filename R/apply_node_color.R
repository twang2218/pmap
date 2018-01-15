#' @importFrom dplyr        %>%
#' @importFrom dplyr        distinct
#' @importFrom dplyr        arrange
#' @importFrom DiagrammeR   get_node_df
#' @importFrom DiagrammeR   select_nodes
#' @importFrom DiagrammeR   get_selection
#' @importFrom DiagrammeR   set_node_attrs_ws
#' @importFrom DiagrammeR   clear_selection
apply_node_color <- function(p) {
  # Make `R CMD check` happy
  type <- NULL

  types <- DiagrammeR::get_node_df(p) %>%
    dplyr::distinct(type) %>%
    dplyr::arrange(type)

  types <- types$type

  colors <- get_colors(types)

  for (t in types) {
    color <- colors[[t]]
    p <- DiagrammeR::select_nodes(p, conditions = type == t)
    if (!any(is.na(DiagrammeR::get_selection(p)))) {
      # print(paste("type:", t))
      # print(paste("color:", color))
      p <- p %>%
        DiagrammeR::set_node_attrs_ws(node_attr = "color", value = color[3]) %>%
        DiagrammeR::set_node_attrs_ws(node_attr = "fillcolor", value = paste0(color[2], ":", color[1]))
    }
    p <- DiagrammeR::clear_selection(p)
  }

  return(p)
}
