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
#'   number_of_customers = 2000,
#'   event_catalogs = c("campaign", "sale"),
#'   event_catalogs_size = c(10, 4))
#' > head(eventlog)
#'             timestamp   customer_id          event_name event_type
#' 1 2017-01-01 02:14:50  Customer 345  Event 1 (campaign)   campaign
#' 2 2017-01-01 02:26:24 Customer 1625  Event 2 (campaign)   campaign
#' 3 2017-01-01 03:48:12 Customer 1901     Event 12 (sale)       sale
#' 4 2017-01-01 03:57:54 Customer 1029 Event 10 (campaign)   campaign
#' 5 2017-01-01 07:46:54  Customer 215 Event 10 (campaign)   campaign
#' 6 2017-01-01 09:44:51 Customer 1354  Event 1 (campaign)   campaign
#' > str(eventlog)
#' 'data.frame':	10000 obs. of  4 variables:
#'  $ timestamp  : POSIXct, format: "2017-01-01 02:14:50" "2017-01-01 02:26:24" ...
#'  $ customer_id: chr  "Customer 345" "Customer 1625" "Customer 1901" "Customer 1029" ...
#'  $ event_name : chr  "Event 1 (campaign)" "Event 2 (campaign)" "Event 12 (sale)" "Event 10 (campaign)" ...
#'  $ event_type : chr  "campaign" "campaign" "sale" "campaign" ...
#' ```
#'
#' Create a process map from the event log and render it directly.
#'
#' ```R
#' > p <- create_pmap(eventlog, target_types = c("sale"))
#' > render_pmap(p)
#' ```
#'
#' \if{html}{\figure{example.prune_edges.none.svg}{options: width="100\%" alt="Figure: example.prune_edges.none.svg"}}
#'
#' As you can see the event map is very messy. Let's apply the `prune_edges()` to remove 50 percent edges.
#'
#' \code{
#'  > p \%>\% prune_edges(0.5) \%>\% render_pmap()
#' }
#'
#' \if{html}{\figure{example.prune_edges.edges.svg}{options: width="100\%" alt="Figure: example.prune_edges.edges.svg"}}
#'
#' It's cleaner, we can clean it further by remove 30 percent nodes with `prune_nodes()` function.
#'
#' \code{
#'  > p \%>\% prune_edges(0.5) \%>\% prune_nodes(0.3) \%>\% render_pmap()
#' }
#'
#' \if{html}{\figure{example.prune_edges.both.svg}{options: width="100\%" alt="Figure: example.prune_edges.both.svg"}}
#'
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
