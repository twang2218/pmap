#' @importFrom DiagrammeR   get_node_df
#' @importFrom DiagrammeR   set_node_attrs
#' @importFrom dplyr        filter
adjust_node_style <- function(p) {
  # make `R CMD Check` happy
  amount <- NULL

  node_df <- DiagrammeR::get_node_df(p)
  nodes_with_amount <- dplyr::filter(node_df, amount > 0)
  if (nrow(nodes_with_amount) > 0) {
    fontsizes <- projection(node_df$amount, 10, 20)
    margins <- paste0(
      projection(node_df$amount, 0.05, 0.3),
      ",",
      projection(node_df$amount, 0.025, 0.15)
    )
    labels <- paste0(node_df$name, "\n(", node_df$amount, ")")
    # found some nodes.
    p <- p %>%
      # Adjust the node size by `amount`, project to `[10, 20]` range
      DiagrammeR::set_node_attrs(
        node_attr = "fontsize",
        values = fontsizes,
        nodes = nodes_with_amount$id
      ) %>%
      # Adjust the node margin by `amount`, project to `[0.05, 0.3]` range
      DiagrammeR::set_node_attrs(
        node_attr = "margin",
        values = margins,
        nodes = nodes_with_amount$id
      ) %>%
      # Attach node label with its `amount`
      DiagrammeR::set_node_attrs(
        node_attr = "label",
        values = labels,
        nodes = nodes_with_amount$id
      )
  }

  p <- apply_node_color(p)

  return (p)
}