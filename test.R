source("process_graph.R")
source("event_log_generator.R")


print("generate_datasets()")
data <- generate_datasets(
  customer_size = 10000,
  campaign_size = 20,
  sales_size = 3
)
print(str(data))

print("generate_event_logs()")
event_logs <- generate_event_logs(
  data = data,
  number_of_campaigns = 30,
  number_of_sales = 500
)
print(str(event_logs))

# event_logs <- event_logs %>% arrange(customer_id, timestamp)
# print(event_logs)

print("generate_graph_data()")
graph_data <- generate_graph_data(event_logs)
print(str(graph_data))

print("create_process_graph()")
p <- create_process_graph(graph_data$nodes, graph_data$links)

print(p)
