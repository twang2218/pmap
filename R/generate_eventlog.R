#' @title Generate random event log
#' @param size_of_eventlog The size of generated event log
#' @param number_of_customers How many customers in the simulation
#' @param event_catalogs A data frame contains the event catalog
#' @param event_catalogs_size How many event types in each event catalog
#' @usage generate_eventlog(
#'      size_of_eventlog = 1000, 
#'      number_of_customers = 20, 
#'      event_catalogs = c("normal", "target"), 
#'      event_catalogs_size = c(8, 2))
#' @description This function provides the ability to randomly generate the `eventlog` data frame based on given parameters.
#' @examples
#' eventlog <- generate_eventlog(
#'      size_of_eventlog = 10000,
#'      number_of_customers = 2000,
#'      event_catalogs = c("campaign", "sale"),
#'      event_catalogs_size = c(10, 4)
#'      )
#'
#' str(eventlog)
#' # 'data.frame':	10000 obs. of  4 variables:
#' #  $ timestamp  : POSIXct, format: "2017-01-01 02:16:16" ...
#' #  $ customer_id: chr  "Customer 107" "Customer 1828" "Customer 587" "Customer 1666" ...
#' #  $ event_name : chr  "Event 4 (campaign)" "Event 11 (sale)" "Event 7 (campaign)" ...
#' #  $ event_type : chr  "campaign" "sale" "campaign" "sale" ...
#' head(eventlog)
#' #             timestamp   customer_id         event_name event_type
#' # 1 2017-01-01 02:16:16  Customer 107 Event 4 (campaign)   campaign
#' # 2 2017-01-01 03:04:22 Customer 1828    Event 11 (sale)       sale
#' # 3 2017-01-01 03:36:35  Customer 587 Event 7 (campaign)   campaign
#' # 4 2017-01-01 05:00:11 Customer 1666    Event 14 (sale)       sale
#' # 5 2017-01-01 05:38:24 Customer 1287    Event 11 (sale)       sale
#' # 6 2017-01-01 05:48:22 Customer 1286 Event 7 (campaign)   campaign
#' @importFrom dplyr  %>%
#' @importFrom dplyr  sample_n
#' @importFrom dplyr  arrange
#' @importFrom stats  runif
#' @export
generate_eventlog <- function(size_of_eventlog = 1000, number_of_customers = 20, event_catalogs = c("normal", "target"), event_catalogs_size = c(8, 2)) {
  # make 'R CMD check' happy
  customer_id <- timestamp <- event_name <- NULL

  # Function for generating random time vector
  generate_random_datetimes <- function(size, from = "2017-01-01", to = "2017-12-31") {
    as.POSIXct(
      round(runif(
        size,
        min = as.numeric(as.POSIXct(from)),
        max = as.numeric(as.POSIXct(to))
      )),
      origin = "1970-01-01"
    )
  }

  # Generate Customers
  customers <- data.frame(
    id = paste0("Customer ", 1:number_of_customers),
    stringsAsFactors = FALSE
  )

  event_types <- data.frame()
  for (i in 1:length(event_catalogs)) {
    event_types_of_catalog <- data.frame(
      name = paste0("Event ", nrow(event_types) + 1:event_catalogs_size[i], " (", event_catalogs[i], ")"),
      type = event_catalogs[i],
      stringsAsFactors = FALSE
    )
    event_types <- rbind(event_types, event_types_of_catalog)
  }

  events <- sample_n(event_types, size_of_eventlog, replace = TRUE)
  eventlog <- data.frame(
    timestamp = generate_random_datetimes(size_of_eventlog),
    customer_id = sample(customers, size_of_eventlog, replace = TRUE)$id,
    event_name = events$name,
    event_type = events$type,
    stringsAsFactors = FALSE
  ) %>% arrange(timestamp, customer_id, event_name)

  return(eventlog)
}
