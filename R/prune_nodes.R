#' @title Prune nodes based on given percentage
#' @param p process map object created by `create_pmap_graph()` function
#' @param percentage how many percentage of the nodes should be pruned.
#' @param rank how to rank the nodes.
#'  `amount` means ranking the nodes by `amount` column (default);
#'  `in_degree` means ranking the nodes by `in_degree`;
#'  `out_degree` means ranking the nodes by `out_degree`;
#' @usage prune_nodes(p, percentage = 0.2, rank = "amount")
#' @description Prune nodes based on given percentage
#' @examples
#' library(dplyr)
#' p <- generate_eventlog() %>% create_pmap(target_types = c("target")) 
#' DiagrammeR::node_count(p)
#' # [1] 10
#' p <- prune_nodes(p, percentage = 0.5)
#' DiagrammeR::node_count(p)
#' # [1] 5
#' @seealso [prune_edges]
#' @importFrom dplyr        %>%
#' @importFrom dplyr        arrange
#' @importFrom DiagrammeR   get_node_df
#' @importFrom DiagrammeR   select_nodes_by_id
#' @importFrom DiagrammeR   delete_nodes_ws
#' @importFrom utils        head
#' @export
prune_nodes <- function(p, percentage = 0.2, rank = "amount") {
  # make 'R CMD Check' happy
  amount <- inbound <- outbound <- NULL

  # Ranking the nodes
  ranked_nodes <- switch(rank,
    amount = get_node_df(p) %>% arrange(amount),
    in_degree = get_node_df(p) %>% arrange(inbound),
    out_degree = get_node_df(p) %>% arrange(outbound)
  )
  # Select the top part of the nodes
  removed_nodes <- ranked_nodes %>% head(percentage * nrow(ranked_nodes))

  # Remove the nodes if it's not empty
  if (nrow(removed_nodes) > 0) {
    p <- p %>%
      select_nodes_by_id(nodes = removed_nodes$id) %>%
      delete_nodes_ws()
  }

  return(p)
}
