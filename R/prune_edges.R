#' @title Prune edges based on given percentage
#' @param p process map object created by `create_pmap()` function
#' @param percentage how many percentage of the edges should be pruned.
#' @description Prune edges based on given percentage
#' @importFrom dplyr        %>%
#' @importFrom dplyr        arrange
#' @importFrom DiagrammeR   get_edge_df
#' @importFrom DiagrammeR   select_edges_by_edge_id
#' @importFrom DiagrammeR   delete_edges_ws
#' @importFrom utils        head
#' @export
prune_edges <- function(p, percentage = 0.2) {
  # make 'R CMD Check' happy
  amount <- NULL

  edf <- get_edge_df(p)

  removed_edges <- edf %>% arrange(amount) %>% head(percentage * nrow(edf))
  if (nrow(removed_edges) > 0) {
    p <- p %>%
      select_edges_by_edge_id(edges = removed_edges$id) %>%
      delete_edges_ws()
  }

  return(p)
}
