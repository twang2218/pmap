#' @title Generate random event log
#' @param size_of_eventlog The size of generated event log
#' @param number_of_cases How many cases in the simulation
#' @param categories A data frame contains the activity category
#' @param categories_size How many activity categories in each activity category
#' @usage generate_eventlog(
#'      size_of_eventlog = 1000, 
#'      number_of_cases = 20, 
#'      categories = c("normal", "visit", "phone", "target"),
#'      categories_size = c(5, 4, 3, 2))
#' @description This function provides the ability to randomly generate the `eventlog` data frame based on given parameters.
#' @examples
#' eventlog <- generate_eventlog(
#'      size_of_eventlog = 10000,
#'      number_of_cases = 2000,
#'      categories = c("campaign", "sale"),
#'      categories_size = c(10, 4)
#'      )
#'
#' str(eventlog)
#' # 'data.frame':	10000 obs. of  4 variables:
#' #  $ timestamp  : POSIXct, format: "2017-01-01 02:16:16" ...
#' #  $ case_id: chr  "Case 107" "Case 1828" "Case 587" "Case 1666" ...
#' #  $ activity : chr  "Activity 4 (campaign)" "Activity 11 (sale)" "Activity 7 (campaign)" ...
#' #  $ category : chr  "campaign" "sale" "campaign" "sale" ...
#' head(eventlog)
#' #             timestamp   case_id         activity category
#' # 1 2017-01-01 02:16:16  Case 107 Activity 4 (campaign)   campaign
#' # 2 2017-01-01 03:04:22 Case 1828    Activity 11 (sale)       sale
#' # 3 2017-01-01 03:36:35  Case 587 Activity 7 (campaign)   campaign
#' # 4 2017-01-01 05:00:11 Case 1666    Activity 14 (sale)       sale
#' # 5 2017-01-01 05:38:24 Case 1287    Activity 11 (sale)       sale
#' # 6 2017-01-01 05:48:22 Case 1286 Activity 7 (campaign)   campaign
#' @importFrom dplyr        %>%
#' @importFrom dplyr        sample_n
#' @importFrom data.table   setorder
#' @importFrom stats        runif
#' @export
generate_eventlog <- function(
  size_of_eventlog = 1000,
  number_of_cases = 20,
  categories = c("normal", "visit", "phone", "target"),
  categories_size = c(5, 4, 3, 2)) {

  # make 'R CMD check' happy
  case_id <- timestamp <- activity <- NULL

  # Generate Cases
  cases <- data.frame(
    id = paste0("Case ", 1:number_of_cases),
    stringsAsFactors = FALSE
  )

  activities <- data.frame()
  for (i in 1:length(categories)) {
    activities_of_category <- data.frame(
      name = paste0("Activity ", nrow(activities) + 1:categories_size[i], " (", categories[i], ")"),
      category = categories[i],
      weight = stats::runif(categories_size[i], 0, 1),
      stringsAsFactors = FALSE
    )
    activities <- rbind(activities, activities_of_category)
  }

  activities <- dplyr::sample_n(activities, size_of_eventlog, replace = TRUE, weight = activities$weight)

  # print(activities %>% group_by(name, weight) %>% summarize(amount = n()))

  eventlog <- data.frame(
    timestamp = generate_random_datetimes(size_of_eventlog),
    case_id = dplyr::sample_n(cases, size_of_eventlog, replace = TRUE)$id,
    activity = activities$name,
    category = activities$category,
    stringsAsFactors = FALSE
  ) %>% data.table::setorder(timestamp, case_id, activity)

  return(eventlog)
}
