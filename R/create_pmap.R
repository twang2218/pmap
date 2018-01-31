#' @title Create the process map from event log directly
#' @usage create_pmap(
#'    eventlog,
#'    distinct_case = FALSE,
#'    distinct_repeated_activities = FALSE,
#'    target_categories = NULL,
#'    edge_label = c(
#'      "amount",
#'      "mean_duration",
#'      "median_duration",
#'      "max_duration",
#'      "min_duration"
#'    )
#'  )
#' @param eventlog Event log
#' @param distinct_case Whether should count distinct case only. Default is `FALSE`.
#' @param distinct_repeated_activities Whether should distinct repeat activities. Default is `FALSE`, which means the repeated activity will be treated as same node. If it's `TRUE`, the name of the activity will be attached with the sequence number of occurance of the activity.
#' @param target_categories A vector contains the target activity categories
#' @param edge_label Specify which attribute is used for the edge label.
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
#'     case_id = c("c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1", "c1"),
#'     activity =  c("a",  "b",  "d",  "a",  "c",  "a",  "b",  "c",  "a",  "d"),
#'     category =  c("campaign", "campaign", "sale", "campaign", "sale", "campaign", "campaign", "sale", "campaign", "sale"),
#'     stringsAsFactors = FALSE
#'   )
#' > eventlog
#'     timestamp case_id activity category
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
#' > p <- create_pmap(eventlog, target_categories = c("sale"))
#' > render_pmap(p)
#' ```
#' \if{html}{\figure{example.create_pmap.simple.svg}{options: alt="Figure: example.create_pmap.simple.svg"}}
#'
#' Or for more complex event log:
#'
#' ```R
#' > eventlog <- generate_eventlog(
#'     size_of_eventlog = 10000,
#'     number_of_cases = 2000,
#'     categories = c("campaign", "sale"),
#'     categories_size = c(8, 2))
#' > head(eventlog)
#'             timestamp   case_id         activity category
#' 1 2017-01-01 02:40:20 Case 1204 Activity 7 (campaign)   campaign
#' 2 2017-01-01 03:10:31 Case 1554 Activity 5 (campaign)   campaign
#' 3 2017-01-01 04:01:51  Case 546 Activity 4 (campaign)   campaign
#' 4 2017-01-01 05:04:09 Case 1119     Activity 9 (sale)       sale
#' 5 2017-01-01 06:43:11 Case 1368 Activity 2 (campaign)   campaign
#' 6 2017-01-01 07:43:06  Case 986 Activity 8 (campaign)   campaign
#' > str(eventlog)
#' 'data.frame':   10000 obs. of  4 variables:
#'  $ timestamp  : POSIXct, format: "2017-01-01 02:40:20" "2017-01-01 03:10:31" ...
#'  $ case_id: chr  "Case 1204" "Case 1554" "Case 546" "Case 1119" ...
#'  $ activity : chr  "Activity 7 (campaign)" "Activity 5 (campaign)" "Activity 4 (campaign)" "Activity 9 (sale)" ...
#'  $ category : chr  "campaign" "campaign" "campaign" "sale" ...
#' > p <- create_pmap(eventlog, target_categories = c("sale"))
#' > render_pmap(p)
#' ```
#' \if{html}{\figure{example.create_pmap.complex.svg}{options: width="100\%" alt="Figure: example.create_pmap.complex.svg"}}
#'
#' @seealso [prune_edges]
#' @seealso [create_pmap_graph]
#' @importFrom dplyr        %>%
#' @importFrom dplyr        group_by
#' @importFrom dplyr        arrange
#' @importFrom dplyr        rename
#' @importFrom dplyr        mutate
#' @importFrom dplyr        ungroup
#' @importFrom stringr      str_trim
#' @export
create_pmap <- function(
  eventlog,
  distinct_case = FALSE,
  distinct_repeated_activities = FALSE,
  target_categories = NULL,
  edge_label = c(
    "amount",
    "mean_duration",
    "median_duration",
    "max_duration",
    "min_duration"
  )
) {
  # Make R Cmd Check happy
  old_activity <- case_id <- activity <- old_activity <- timestamp <- NULL

  # clean names
  eventlog <- eventlog %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    dplyr::mutate_if(is.character, stringr::str_trim)

  if (distinct_repeated_activities) {
    eventlog <- eventlog %>%
      dplyr::group_by(case_id, activity) %>%
      dplyr::arrange(timestamp, .by_group = TRUE) %>%
      dplyr::rename(old_activity = activity) %>%
      dplyr::mutate(activity = paste0(old_activity, " (", 1:n(), ")")) %>%
      dplyr::ungroup()

    if (!"category" %in% colnames(eventlog)) {
      # if the `category` is missing,
      # then use the original `activity` as the `category`
      eventlog <- eventlog %>% dplyr::rename(category = old_activity)
    }
  }

  nodes <- generate_nodes(eventlog, distinct_case)
  edges <- generate_edges(eventlog, distinct_case, target_categories)

  if (nrow(nodes) == 0) {
    stop("Generated graph contains empty node.")
  }

  if (nrow(edges) == 0) {
    stop("Generated graph contains no edge.")
  }

  p <- create_pmap_graph(nodes, edges, target_categories, edge_label)
  return(p)
}
