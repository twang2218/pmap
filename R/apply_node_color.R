#' @importFrom dplyr        %>%
#' @importFrom dplyr        distinct
#' @importFrom dplyr        inner_join
#' @importFrom DiagrammeR   get_node_df
#' @importFrom DiagrammeR   set_node_attrs
apply_node_color <- function(p) {
  # Make `R CMD check` happy
  type <- id <- NULL

  node_df <- DiagrammeR::get_node_df(p) %>% select(id, type)

  types <- dplyr::distinct(node_df, type)$type

  if (any(is.na(types))) {
    # we don't handle the types with `NA`
    return(p)
  }

  nodes <- dplyr::inner_join(node_df, get_colors(types), by = "type")

  if (nrow(nodes) > 0) {
    p <- DiagrammeR::set_node_attrs(p, node_attr = "color", values = nodes$color, nodes = nodes$id)
    p <- DiagrammeR::set_node_attrs(p, node_attr = "fillcolor", values = nodes$fillcolor, nodes = nodes$id)
  }

  return(p)
}
