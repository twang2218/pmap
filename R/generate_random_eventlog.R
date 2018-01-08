#' @title Generate Random Eventlog
#' @param size_of_eventlog The size of generated event log
#' @param number_of_customers How many customers in the simulation
#' @param event_catalogs A data frame contains the event catelog
#' @param event_catalogs_size How many event types in each event catalog
#' @usage generate_random_eventlog(
#'      size_of_eventlog = 1000, 
#'      number_of_customers = 20, 
#'      event_catalogs = c("normal", "target"), 
#'      event_catalogs_size = c(8, 2))
#' @description This function provides the ability to randomly generate the eventlog data frame based on given parameters.
#' @details
#' Example
#' ```R
#' > eventlog <- generate_random_eventlog(500, 20)
#' > str(eventlog)
#' 'data.frame':	500 obs. of  4 variables:
#'  $ timestamp  : POSIXct, format: "2017-01-01 03:54:47" "2017-01-01 20:31:02" ...
#'  $ customer_id: chr  "Customer 20" "Customer 20" "Customer 1" "Customer 3" ...
#'  $ event_name : chr  "Event 10 (target)" "Event 6 (normal)" "Event 8 (normal)" "Event 9 (target)" ...
#'  $ event_type : chr  "target" "normal" "normal" "target" ...
#' ```
#' @importFrom dplyr  %>%
#' @importFrom dplyr  sample_n
#' @importFrom dplyr  arrange
#' @importFrom stats  runif
#' @export
generate_random_eventlog <- function(size_of_eventlog = 1000, number_of_customers = 20, event_catalogs = c("normal", "target"), event_catalogs_size = c(8, 2)) {
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
    stringsAsFactors = F
  )

  event_types <- data.frame()
  for (i in 1:length(event_catalogs)) {
    event_types_of_catalog <- data.frame(
      name = paste0("Event ", nrow(event_types) + 1:event_catalogs_size[i], " (", event_catalogs[i], ")"),
      type = event_catalogs[i],
      stringsAsFactors = F
    )
    event_types <- rbind(event_types, event_types_of_catalog)
  }

  events <- sample_n(event_types, size_of_eventlog, replace = T)
  eventlog <- data.frame(
    timestamp = generate_random_datetimes(size_of_eventlog),
    customer_id = sample(customers, size_of_eventlog, replace = T)$id,
    event_name = events$name,
    event_type = events$type,
    stringsAsFactors = F
  ) %>% arrange(timestamp, customer_id, event_name)

  return(eventlog)
}
