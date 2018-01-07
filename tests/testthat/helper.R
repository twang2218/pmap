library(dplyr)
library(data.table)


# Generate Random Time
generate_random_datetimes <- function(size, from = "2017-09-01", to = "2017-10-01") {
  return(
    as.POSIXct(
      round(runif(
        size,
        min = as.numeric(as.POSIXct(from)),
        max = as.numeric(as.POSIXct(to))
      )),
      origin = "1970-01-01"
    )
  )
}



# Generate Datasets
generate_datasets <- function(customer_size, campaign_size, sales_size) {
  ### Groups
  # customer group
  customers <- data.table(
    id = paste0("Customer ", 1:customer_size),
    stringsAsFactors = F
  )
  # print(customers)

  # campaigns
  campaigns <- data.table(
    name = paste0("Campaign ", 1:campaign_size),
    type = "campaign",
    timestamp = generate_random_datetimes(campaign_size, "2017-09-01", "2017-10-01"),
    amount = ceiling(runif(campaign_size, min = 0.001, max = 0.03) * customer_size),
    stringsAsFactors = F
  )
  # print(campaigns)

  # sales group
  sales <- data.table(
    name = paste0("Sale ", 1:sales_size),
    type = "sale",
    timestamp = generate_random_datetimes(sales_size, "2017-09-10", "2017-10-01"),
    amount = ceiling(runif(sales_size, min = 0.05, max = 0.1) * customer_size),
    stringsAsFactors = F
  )
  # print(sales)

  datasets <- list(
    customers = customers,
    events = rbind(campaigns, sales)
  )

  return(datasets)
}



# Simulate sending campaign
send_campaign <- function(customers, campaign) {
  event_logs_for_the_campaign <- data.table(
    timestamp = campaign$timestamp,
    customer_id = sample_n(customers, campaign$amount)$id,
    event_name = campaign$name,
    event_type = campaign$type,
    stringsAsFactors = F
  )

  return(event_logs_for_the_campaign)
}




# Simulate sales
generate_sales <- function(customers, sale) {
  event_logs_for_the_sale <- data.table(
    timestamp = generate_random_datetimes(sale$amount, sale$timestamp, "2017-10-01"),
    customer_id = sample_n(customers, sale$amount)$id,
    event_name = sale$name,
    event_type = sale$type,
    stringsAsFactors = F
  )

  return(event_logs_for_the_sale)
}


# Generate event logs
generate_eventlog <- function(data, number_of_campaigns, number_of_sales) {
  # Generate Logs

  eventlog <- list()

  # Send multiple campaigns
  campaigns_eventlog <- list()
  campaigns <- data$events[data$events$type == "campaign", ]
  for (i in 1:nrow(campaigns)) {
    # put everything into a list is more efficient than `rbind()`
    campaigns_eventlog[[length(campaigns_eventlog) + 1]] <- send_campaign(data$customers, campaigns[i, ])
  }
  campaigns_eventlog <- rbindlist(campaigns_eventlog)

  targeted_customers <- campaigns_eventlog %>%
    distinct(customer_id) %>%
    rename(id = customer_id) %>%
    arrange(id)

  # Generate multiple sales
  sales_eventlog <- list()
  sales <- data$events[data$events$type == "sale",]
  for (i in 1:nrow(sales)) {
    sales_eventlog[[length(sales_eventlog) + 1]] <- generate_sales(targeted_customers, sales[i, ])
  }
  sales_eventlog <- rbindlist(sales_eventlog)

  # merge all eventlog
  eventlog <- rbind(campaigns_eventlog, sales_eventlog) %>% arrange(timestamp)

  return(eventlog)
}
