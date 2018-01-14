#' @importFrom DiagrammeR   get_node_df
#' @importFrom DiagrammeR   set_node_attrs
#' @importFrom dplyr        filter
adjust_node_style <- function(p) {
  # make `R CMD Check` happy
  amount <- NULL

  node_df <- DiagrammeR::get_node_df(p)
  if ("amount" %in% colnames(node_df)) {
    nodes_with_amount <- node_df %>% dplyr::filter(amount >= 0)
    if (nrow(nodes_with_amount) > 0) {
      fontsizes <- projection(nodes_with_amount$amount, 10, 20)
      labels <- paste0(nodes_with_amount$name, "\n(", nodes_with_amount$amount, ")")
      # found some nodes.
      p <- p %>%
        # Adjust the node size by `amount`, project to `[10, 20]` range
        DiagrammeR::set_node_attrs(
          node_attr = "fontsize",
          values = fontsizes,
          nodes = nodes_with_amount$id
        ) %>%
        # Attach node label with its `amount`
        DiagrammeR::set_node_attrs(
          node_attr = "label",
          values = labels,
          nodes = nodes_with_amount$id
        )
    }
  }

  p <- apply_node_color(p)

  return (p)
}