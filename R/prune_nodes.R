#' @title Prune nodes based on given percentage
#' @param p process map object created by `create_pmap()` function
#' @param percentage how many percentage of the nodes should be pruned.
#' @param rank how to rank the nodes.
#'  `count` means ranking the nodes by `count` column (default);
#'  `in_degree` means ranking the nodes by `in_degree`;
#'  `out_degree` means ranking the nodes by `out_degree`;
#' @usage prune_nodes(p, percentage = 0.2, rank = "count")
#' @description Prune nodes based on given percentage
#' @importFrom dplyr        %>%
#' @importFrom dplyr        arrange
#' @importFrom DiagrammeR   get_node_df
#' @importFrom DiagrammeR   select_nodes_by_id
#' @importFrom DiagrammeR   delete_nodes_ws
#' @importFrom utils        head
#' @export
prune_nodes <- function(p, percentage = 0.2, rank = "count") {
  # make 'R CMD Check' happy
  value <- count <- inbound <- outbound <- NULL

  # Ranking the nodes
  ranked_nodes <- switch(rank,
    count = get_node_df(p) %>% arrange(count),
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
