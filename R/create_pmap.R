#' @title Create the process map from event log directly
#' @usage create_pmap(eventlog, distinct_customer = FALSE, target_types = NULL)
#' @param eventlog Event log
#' @param distinct_customer Whether should count distinct customer only. Default is `FALSE`.
#' @param target_types A vector contains the target event types
#' @description Create the process map by analyzing the given `eventlog` and extract the nodes by `generate_nodes()` and edges by `generate_edges()`.
#' @details
#' ```R
#' > eventlog <- data.frame(
#'     timestamp = c(
#'       as.POSIXct("2017-10-01"),
#'       as.POSIXct("2017-10-02"),
#'       as.POSIXct("2017-10-03"),
#'       as.POSIXct("2017-10-04"),
#'       as.POSIXct("2017-10-05"),
#'       as.POSIXct("2017-10-06"),
#'       as.POSIXct("2017-10-07"),
#'       as.POSIXct("2017-10-08"),
#'       as.POSIXct("2017-10-09"),
#'       as.POSIXct("2017-10-10")
#'     ),
#'     customer_id = c("c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1"),
#'     event_name =  c("a",  "b",  "d",  "a",  "c",  "a",  "b",  "c",  "a",  "d"),
#'     event_type =  c("campaign", "campaign", "sale", "campaign", "sale", "campaign", "campaign", "sale", "campaign", "sale"),
#'     stringsAsFactors = FALSE
#'   )
#' > eventlog
#'     timestamp customer_id event_name event_type
#' 1  2017-10-01          c1          a   campaign
#' 2  2017-10-02          c1          b   campaign
#' 3  2017-10-03          c1          d       sale
#' 4  2017-10-04          c1          a   campaign
#' 5  2017-10-05          c1          c       sale
#' 6  2017-10-06          c1          a   campaign
#' 7  2017-10-07          c1          b   campaign
#' 8  2017-10-08          c1          c       sale
#' 9  2017-10-09          c1          a   campaign
#' 10 2017-10-10          c1          d       sale
#' > p <- create_pmap(eventlog, target_types = c("sale"))
#' > render_pmap(p)
#' ```
#' \if{html}{\figure{example.create_pmap.simple.svg}{options: alt="Figure: example.create_pmap.simple.svg"}}
#'
#' Or for more complex event log:
#'
#' ```R
#' > eventlog <- generate_eventlog(
#'   size_of_eventlog = 10000, 
#'   number_of_customers = 2000, 
#'   event_catalogs = c("campaign", "sale"), 
#'   event_catalogs_size = c(8, 2))
#' > head(eventlog)
#'             timestamp   customer_id         event_name event_type
#' 1 2017-01-01 02:40:20 Customer 1204 Event 7 (campaign)   campaign
#' 2 2017-01-01 03:10:31 Customer 1554 Event 5 (campaign)   campaign
#' 3 2017-01-01 04:01:51  Customer 546 Event 4 (campaign)   campaign
#' 4 2017-01-01 05:04:09 Customer 1119     Event 9 (sale)       sale
#' 5 2017-01-01 06:43:11 Customer 1368 Event 2 (campaign)   campaign
#' 6 2017-01-01 07:43:06  Customer 986 Event 8 (campaign)   campaign
#' > str(eventlog)
#' 'data.frame':   10000 obs. of  4 variables:
#'  $ timestamp  : POSIXct, format: "2017-01-01 02:40:20" "2017-01-01 03:10:31" ...
#'  $ customer_id: chr  "Customer 1204" "Customer 1554" "Customer 546" "Customer 1119" ...
#'  $ event_name : chr  "Event 7 (campaign)" "Event 5 (campaign)" "Event 4 (campaign)" "Event 9 (sale)" ...
#'  $ event_type : chr  "campaign" "campaign" "campaign" "sale" ...
#' > p <- create_pmap(eventlog, target_types = c("sale"))
#' > render_pmap(p)
#' ```
#' \if{html}{\figure{example.create_pmap.complex.svg}{options: width="100\%" alt="Figure: example.create_pmap.complex.svg"}}
#'
#' @seealso [prune_edges]
#' @export
create_pmap <- function(eventlog, distinct_customer = FALSE, target_types = NULL) {
  nodes <- generate_nodes(eventlog, distinct_customer)
  edges <- generate_edges(eventlog, distinct_customer, target_types)
  p <- create_pmap_graph(nodes, edges, target_types)
  return(p)
}
