library(pmap)
library(DiagrammeR)
source("helper.R")

set.seed(101)

context("create_pmap()")


test_that("create_pmap()", {
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

  # print("generate_eventlog()")
  eventlog <- generate_eventlog(
    data = data,
    number_of_campaigns = 12,
    number_of_sales = 5000
  )

  expect_named(eventlog, c("timestamp", "customer_id", "event_name", "event_type", "is_target"), ignore.order = TRUE, ignore.case = TRUE)
  expect_gt(nrow(eventlog), 1000)

  # print(str(eventlog))

  # print("generate_edges()")
  edges <- generate_edges(eventlog)

  expect_named(edges, c("from", "to", "value"))
  expect_gt(nrow(edges), 10)

  # print(str(edges))

  # print("create_pmap()")
  p <- create_pmap(data$events, edges, render = F)

  edges_from_graph <- get_edge_df(p)
  expect_equal(nrow(edges), nrow(edges_from_graph))

  # print("render_graph()")
  expect_true(!any(is.null(render_graph(p))))
  # print(render_graph(p))
})
