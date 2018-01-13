#' @title Apply node colors based on the node's type
#' @description Different type of node should be differenciate by different colors, this function applies the material design palette for the node's color.
#' @usage apply_node_color(p)
#' @param p process map generated `by create_pmap()`
#' @importFrom dplyr        %>%
#' @importFrom dplyr        distinct
#' @importFrom dplyr        arrange
#' @importFrom DiagrammeR   get_node_df
#' @importFrom ggsci        pal_material
#' @importFrom grDevices    col2rgb
#' @importFrom grDevices    rgb
apply_node_color <- function(p) {
  # Make `R CMD check` happy
  type <- NULL

  types <- get_node_df(p) %>%
    distinct(type) %>%
    arrange(type)

  types <- types$type

  colors <- get_colors(types)

  for (t in types) {
    color <- colors[[t]]
    p <- select_nodes(p, conditions = type == t)
    if (!any(is.na(get_selection(p)))) {
      # print(paste("type:", t))
      # print(paste("color:", color))
      p <- p %>%
        set_node_attrs_ws(node_attr = "color", value = color[3]) %>%
        set_node_attrs_ws(node_attr = "fillcolor", value = paste0(color[2], ":", color[1]))
    }
    p <- clear_selection(p)
  }

  return(p)
}
