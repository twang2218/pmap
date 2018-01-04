library(pmap)

context("General Test")

source("event_log_generator.R")

test_that("Create a full graph", {
  # print("generate_datasets()")

  customer_size <- 100000
  campaign_size <- 10
  sales_size <- 3

  data <- generate_datasets(
    customer_size = customer_size,
    campaign_size = campaign_size,
    sales_size = sales_size
  )

  expect_named(data, c("customers", "events"))
  expect_equal(nrow(data$customers), customer_size)
  expect_equal(nrow(data$events), campaign_size + sales_size)

  # print(str(data))

  # print("generate_event_logs()")
  event_logs <- generate_event_logs(
    data = data,
    number_of_campaigns = 12,
    number_of_sales = 5000
  )

  expect_named(event_logs, c("timestamp", "customer_id", "event_name", "event_type", "is_target"), ignore.order = TRUE, ignore.case = TRUE)
  expect_gt(nrow(event_logs), 1000)

  # print(str(event_logs))

  # print("get_edges_from_event_logs()")
  edges <- get_edges_from_event_logs(event_logs)

  expect_named(edges, c("from", "to", "value"))
  expect_gt(nrow(edges), 10)

  # print(str(edges))

  # print("create_process_graph()")
  p <- create_event_graph(data$events, edges)

  edges_from_graph <- DiagrammeR::get_edge_df(p)
  expect_equal(nrow(edges), nrow(edges_from_graph))

  # print("render_graph()")
  expect_true(!any(is.null(DiagrammeR::render_graph(p))))
  # print(DiagrammeR::render_graph(p))
})
