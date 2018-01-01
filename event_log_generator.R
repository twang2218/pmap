library(dplyr)

set.seed(101)

# Generate Datasets
generate_datasets <- function(customer_size, campaign_size, sales_size) {
  ### Groups
  # customer group
  customers <- data.table(
    id = paste0("Customer_", 1:customer_size),
    stringsAsFactors = F
  )
  # print(customers)

  # campaigns
  campaigns <- data.table(
    name = paste0("Campaign_", 1:campaign_size),
    type = "campaign",
    is_target = F,
    stringsAsFactors = F
  )
  # print(campaigns)

  # sales group
  sales <- data.table(
    name = paste0("Sale_", 1:sales_size),
    type = "sale",
    is_target = T,
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
send_campaign <- function(customers, campaigns) {
  # send a campaign
  ## select a campaign
  campaign <- sample_n(campaigns, 1)

  ## select customer target group
  percentage_of_customers <- runif(1, min=0.3, max=1)
  customer_target_group <- sample_frac(customers, percentage_of_customers)

  timestamp <- round(runif(1, min = 100, max=10000))
  event_logs_for_the_campaign <- data.table(
    timestamp = timestamp,
    customer_id = customer_target_group$id,
    event_name = campaign$name,
    event_type = campaign$type,
    is_target = F,
    stringsAsFactors = F
  )
  # print(str(event_logs_for_the_campaign))
  return(event_logs_for_the_campaign)
}

# Simulate sales
generate_sales <- function(customers, sales, sales_size) {
  # Generate random timestamps
  timestamps <- round(runif(sales_size, min = 5000, max = 10000))
  # Random pick customers
  sold_customers <- sample_n(customers, sales_size)
  # Random Pick sales
  sold_sales <- sample_n(sales, sales_size, replace = TRUE)
  
  event_logs_for_the_sale <- data.table(
    timestamp = timestamps,
    customer_id = sold_customers$id,
    event_name = sold_sales$name,
    event_type = sold_sales$type,
    is_target = T,
    stringsAsFactors = F
  )

  return(event_logs_for_the_sale)
}


# Generate event logs
generate_event_logs <- function(data, number_of_campaigns, number_of_sales) {
  # Generate Logs

  event_logs <- list()

  # Send multiple campaigns
  campaigns <- data$events[data$events$type == "campaign",]
  for (i in 0:number_of_campaigns) {
    # put everything into a list is more efficient than `rbind()`
    event_logs[[length(event_logs) + 1]] <- send_campaign(data$customers, campaigns)
  }

  # Generate multiple sales
  sales <- data$events[data$events$type == "sale",]

  event_logs[[length(event_logs) + 1]] <- generate_sales(data$customers, sales, number_of_sales)

  # merge all event_logs
  event_logs <- rbindlist(event_logs) %>% arrange(timestamp)

  return(event_logs)
}
