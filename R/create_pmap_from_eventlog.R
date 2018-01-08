#' @title Create the process map from event log directly
#' @usage create_pmap_from_eventlog(eventlog, distinct_customer = F, target_types = NULL)
#' @param eventlog Event log
#' @param distinct_customer Whether should count distinct customer only. Default is `FALSE`.
#' @param target_types A vector contains the target event types
#' @description Create the process map by analysing given `eventlog` and extract the nodes by `generate_nodes()` and edges by `generate_edges()`.
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
#' > p <- create_pmap_from_eventlog(eventlog, target_types = c("sale"))
#' > render_pmap(p)
#' ```
#' \if{html}{\figure{example_create_pmap_from_eventlog.png}{options: alt="Figure: example_create_pmap_from_eventlog.png"}}
#' @export
create_pmap_from_eventlog <- function(eventlog, distinct_customer = F, target_types = NULL) {
  nodes <- generate_nodes(eventlog, distinct_customer)
  edges <- generate_edges(eventlog, distinct_customer, target_types)
  p <- create_pmap(nodes, edges, target_types)
  return(p)
}
