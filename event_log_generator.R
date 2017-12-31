library(dplyr)

set.seed(101)

# Generate Datasets
generate_datasets <- function(customer_size, campaign_size, sales_size) {
  ### Groups
  # customer group
  customer_names <- paste0("Customer_", LETTERS[1:customer_size])
  customers <- data.table(
    customer_id = 1:length(customer_names),
    customer_name = customer_names,
    stringsAsFactors = F
  )
  # print(customers)

  # campaign group
  campaign_names <- paste0("Campaign_", LETTERS[1:campaign_size])
  campaigns <- data.table(
    campaign_id = 1:length(campaign_names),
    campaign_name = campaign_names,
    is_target = F,
    stringsAsFactors = F
  )
  # print(campaigns)

  # sales group
  sales_names <- paste0("Sales_", LETTERS[1:sales_size])
  sales <- data.table(
    sales_id = 10000 + 1:length(sales_names),
    sales_name = sales_names,
    is_target = T,
    stringsAsFactors = F
  )
  # print(sales)

  datasets <- list(
    customers = customers,
    campaigns = campaigns,
    sales = sales
  )

  return(datasets)
}

# Simulate sending campaign
send_campaign <- function(customers, campaigns) {
  # send a campaign
  ## select a campaign
  campaign <- sample_n(campaigns, 1)

  ## select target group
  percentage_of_customers <- runif(1, min=0.3, max=1)
  target_group <- sample_frac(customers, percentage_of_customers)

  timestamp <- round(runif(1, min = 100, max=10000))
  # print(paste("timestamp = ", timestamp))
  event_logs_for_the_campaign <- data.table(
    timestamp = timestamp,
    customer_id = target_group$customer_id,
    customer_name = target_group$customer_name,
    event_id = campaign$campaign_id,
    event_name = campaign$campaign_name,
    is_target = F,
    stringsAsFactors = F
  )
  # print(str(event_logs_for_the_campaign))
  return(event_logs_for_the_campaign)
}

# Simulate sales
generate_sales <- function(customers, sales, sales_size) {
  timestamps <- round(runif(sales_size, min = 5000, max = 10000))
  sold_customers <- sample_n(customers, sales_size)
  sold_sales <- sample_n(sales, sales_size, replace = TRUE)
  
  event_logs_for_the_sale <- data.table(
    timestamp = timestamps,
    customer_id = sold_customers$customer_id,
    customer_name = sold_customers$customer_name,
    event_id = sold_sales$sales_id,
    event_name = sold_sales$sales_name,
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
  for (i in 0:number_of_campaigns) {
    # put everything into a list is more efficient than `rbind()`
    event_logs[[length(event_logs) + 1]] <- send_campaign(data$customers, data$campaigns)
  }

  # Generate multiple sales
  event_logs[[length(event_logs) + 1]] <- generate_sales(data$customers, data$sales, number_of_sales)

  # for(i in 0:number_of_sales) {
  #   # put everything into a list is more efficient than `rbind()`
  #   event_logs[[length(event_logs) + 1]] <- generate_sales(data$customers, data$sales)
  # }

  event_logs <- rbindlist(event_logs) %>% arrange(timestamp)

  return(event_logs)
}
