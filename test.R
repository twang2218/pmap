source("process_graph.R")
source("event_log_generator.R")


print("generate_datasets()")
data <- generate_datasets(
  customer_size = 10000,
  campaign_size = 10,
  sales_size = 3
)
# print(str(data))

print("generate_event_logs()")
event_logs <- generate_event_logs(
  data = data,
  number_of_campaigns = 12,
  number_of_sales = 5000
)
# print(str(event_logs))

print("get_edges_from_event_logs()")
edges <- get_edges_from_event_logs(event_logs)
# print(str(edges))

print("create_process_graph()")
p <- create_event_graph(data$events, edges)

print("render_graph()")
print(render_graph(p))
