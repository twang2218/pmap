#' @title Prune edges based on given percentage
#' @param p process map object created by `create_pmap_graph()` function
#' @param percentage how many percentage of the edges should be pruned.
#' @description Prune edges based on given percentage
#' @details
#' Create an event log
#' ```R
#' > library(dplyr)
#' > library(pmap)
#' > eventlog <- generate_eventlog(
#'   size_of_eventlog = 10000,
#'   number_of_cases = 2000,
#'   categories = c("campaign", "sale"),
#'   categories_size = c(10, 4))
#' > head(eventlog)
#'             timestamp   case_id          activity category
#' 1 2017-01-01 02:14:50  Case 345  Activity 1 (campaign)   campaign
#' 2 2017-01-01 02:26:24 Case 1625  Activity 2 (campaign)   campaign
#' 3 2017-01-01 03:48:12 Case 1901     Activity 12 (sale)       sale
#' 4 2017-01-01 03:57:54 Case 1029 Activity 10 (campaign)   campaign
#' 5 2017-01-01 07:46:54  Case 215 Activity 10 (campaign)   campaign
#' 6 2017-01-01 09:44:51 Case 1354  Activity 1 (campaign)   campaign
#' > str(eventlog)
#' 'data.frame':	10000 obs. of  4 variables:
#'  $ timestamp  : POSIXct, format: "2017-01-01 02:14:50" "2017-01-01 02:26:24" ...
#'  $ case_id: chr  "Case 345" "Case 1625" "Case 1901" "Case 1029" ...
#'  $ activity : chr  "Activity 1 (campaign)" "Activity 2 (campaign)" "Activity 12 (sale)" "Activity 10 (campaign)" ...
#'  $ category : chr  "campaign" "campaign" "sale" "campaign" ...
#' ```
#'
#' Create a process map from the event log and render it directly.
#'
#' ```R
#' > p <- create_pmap(eventlog, target_categories = c("sale"))
#' > render_pmap(p)
#' ```
#'
#' \if{html}{\figure{example.prune_edges.none.svg}{options: width="100\%" height="400px" alt="Figure: example.prune_edges.none.svg"}}
#'
#' As you can see the activity map is very messy. Let's apply the `prune_edges()` to remove 50 percent edges.
#'
#' \code{
#'  > p \%>\% prune_edges(0.5) \%>\% render_pmap()
#' }
#'
#' \if{html}{\figure{example.prune_edges.edges.svg}{options: width="100\%" height="500px" alt="Figure: example.prune_edges.edges.svg"}}
#'
#' It's cleaner, we can clean it further by remove 50 percent nodes with `prune_nodes()` function.
#'
#' \code{
#'  > p \%>\% prune_nodes(0.5) \%>\% prune_edges(0.5) \%>\% render_pmap()
#' }
#'
#' \if{html}{\figure{example.prune_edges.both.svg}{options: width="100\%" height="500px" alt="Figure: example.prune_edges.both.svg"}}
#'
#' One thing should be noticed, the order of pruning nodes and edges matters.
#' @importFrom dplyr        %>%
#' @importFrom data.table   setorder
#' @importFrom DiagrammeR   get_edge_df
#' @importFrom DiagrammeR   select_edges_by_edge_id
#' @importFrom DiagrammeR   delete_edges_ws
#' @importFrom utils        head
#' @export
prune_edges <- function(p, percentage = 0.2) {
  # make 'R CMD Check' happy
  amount <- NULL

  edf <- DiagrammeR::get_edge_df(p)

  removed_edges <- edf %>% data.table::setorder(amount) %>% head(round(percentage * nrow(edf)))
  if (nrow(removed_edges) > 0) {
    p <- p %>%
      DiagrammeR::select_edges_by_edge_id(edges = removed_edges$id) %>%
      DiagrammeR::delete_edges_ws()
  }

  p <- clean_graph(p)

  return(p)
}
