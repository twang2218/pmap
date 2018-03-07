#' @title Prune nodes based on given percentage
#' @param p process map object created by `create_pmap_graph()` function
#' @param percentage how many percentage of the nodes should be pruned.
#' @param rank how to rank the nodes.
#'  `amount` means ranking the nodes by `amount` column (default);
#'  `in_degree` means ranking the nodes by `in_degree`;
#'  `out_degree` means ranking the nodes by `out_degree`;
#' @usage prune_nodes(p, percentage = 0.2, rank = c("amount", "in_degree", "out_degree"))
#' @description Prune nodes based on given percentage
#' @examples
#' library(dplyr)
#' p <- generate_eventlog() %>% create_pmap(target_categories = c("target"))
#' DiagrammeR::count_nodes(p)
#' # [1] 10
#' p <- prune_nodes(p, percentage = 0.5)
#' DiagrammeR::count_nodes(p)
#' # [1] 5
#' @seealso [prune_edges]
#' @importFrom dplyr        %>%
#' @importFrom data.table   setorder
#' @importFrom DiagrammeR   get_node_df
#' @importFrom DiagrammeR   select_nodes_by_id
#' @importFrom DiagrammeR   delete_nodes_ws
#' @importFrom utils        head
#' @export
prune_nodes <- function(p, percentage = 0.2, rank = c("amount", "in_degree", "out_degree")) {
  # make 'R CMD Check' happy
  amount <- inbound <- outbound <- NULL

  rank <- match.arg(rank)

  node_df <- DiagrammeR::get_node_df(p)

  # Ranking the nodes
  ranked_nodes <- switch(rank,
    amount = data.table::setorder(node_df, amount),
    in_degree = data.table::setorder(node_df, inbound),
    out_degree = data.table::setorder(node_df, outbound)
  )
  # Select the top part of the nodes
  removed_nodes <-  head(ranked_nodes, round(percentage * nrow(ranked_nodes)))

  # Remove the nodes if it's not empty
  if (nrow(removed_nodes) > 0) {
    p <- p %>%
      DiagrammeR::select_nodes_by_id(nodes = removed_nodes$id) %>%
      DiagrammeR::delete_nodes_ws()
  }

  p <- clean_graph(p)

  return(p)
}
