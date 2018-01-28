#' @importFrom dplyr        %>%
#' @importFrom dplyr        distinct
#' @importFrom dplyr        inner_join
#' @importFrom DiagrammeR   get_node_df
#' @importFrom DiagrammeR   set_node_attrs
apply_node_color <- function(p) {
  # Make `R CMD check` happy
  type <- category <- id <- NULL

  node_df <- DiagrammeR::get_node_df(p) %>% select(id, type) %>% rename(category = type)

  categories <- dplyr::distinct(node_df, category)$category

  if (any(is.na(categories))) {
    # we don't handle the categories with `NA`
    return(p)
  }

  nodes <- dplyr::inner_join(node_df, get_colors(categories), by = "category")

  if (nrow(nodes) > 0) {
    p <- DiagrammeR::set_node_attrs(p, node_attr = "color", values = nodes$color, nodes = nodes$id)
    p <- DiagrammeR::set_node_attrs(p, node_attr = "fillcolor", values = nodes$fillcolor, nodes = nodes$id)
  }

  return(p)
}
