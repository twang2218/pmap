#' @title Generate random event log
#' @param size_of_eventlog The size of generated event log
#' @param number_of_cases How many cases in the simulation
#' @param event_categories A data frame contains the event category
#' @param event_categories_size How many event categories in each event category
#' @usage generate_eventlog(
#'      size_of_eventlog = 1000, 
#'      number_of_cases = 20, 
#'      event_categories = c("normal", "visit", "phone", "target"),
#'      event_categories_size = c(5, 4, 3, 2))
#' @description This function provides the ability to randomly generate the `eventlog` data frame based on given parameters.
#' @examples
#' eventlog <- generate_eventlog(
#'      size_of_eventlog = 10000,
#'      number_of_cases = 2000,
#'      event_categories = c("campaign", "sale"),
#'      event_categories_size = c(10, 4)
#'      )
#'
#' str(eventlog)
#' # 'data.frame':	10000 obs. of  4 variables:
#' #  $ timestamp  : POSIXct, format: "2017-01-01 02:16:16" ...
#' #  $ case_id: chr  "Case 107" "Case 1828" "Case 587" "Case 1666" ...
#' #  $ event_name : chr  "Event 4 (campaign)" "Event 11 (sale)" "Event 7 (campaign)" ...
#' #  $ event_category : chr  "campaign" "sale" "campaign" "sale" ...
#' head(eventlog)
#' #             timestamp   case_id         event_name event_category
#' # 1 2017-01-01 02:16:16  Case 107 Event 4 (campaign)   campaign
#' # 2 2017-01-01 03:04:22 Case 1828    Event 11 (sale)       sale
#' # 3 2017-01-01 03:36:35  Case 587 Event 7 (campaign)   campaign
#' # 4 2017-01-01 05:00:11 Case 1666    Event 14 (sale)       sale
#' # 5 2017-01-01 05:38:24 Case 1287    Event 11 (sale)       sale
#' # 6 2017-01-01 05:48:22 Case 1286 Event 7 (campaign)   campaign
#' @importFrom dplyr        %>%
#' @importFrom dplyr        sample_n
#' @importFrom data.table   setorder
#' @importFrom stats        runif
#' @export
generate_eventlog <- function(
  size_of_eventlog = 1000,
  number_of_cases = 20,
  event_categories = c("normal", "visit", "phone", "target"),
  event_categories_size = c(5, 4, 3, 2)) {

  # make 'R CMD check' happy
  case_id <- timestamp <- event_name <- NULL

  # Generate Cases
  cases <- data.frame(
    id = paste0("Case ", 1:number_of_cases),
    stringsAsFactors = FALSE
  )

  event_names <- data.frame()
  for (i in 1:length(event_categories)) {
    event_names_of_category <- data.frame(
      name = paste0("Event ", nrow(event_names) + 1:event_categories_size[i], " (", event_categories[i], ")"),
      category = event_categories[i],
      weight = stats::runif(event_categories_size[i], 0, 1),
      stringsAsFactors = FALSE
    )
    event_names <- rbind(event_names, event_names_of_category)
  }

  events <- dplyr::sample_n(event_names, size_of_eventlog, replace = TRUE, weight = event_names$weight)

  # print(events %>% group_by(name, weight) %>% summarize(amount = n()))

  eventlog <- data.frame(
    timestamp = generate_random_datetimes(size_of_eventlog),
    case_id = dplyr::sample_n(cases, size_of_eventlog, replace = TRUE)$id,
    event_name = events$name,
    event_category = events$category,
    stringsAsFactors = FALSE
  ) %>% data.table::setorder(timestamp, case_id, event_name)

  return(eventlog)
}
