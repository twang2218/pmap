library(pmap)
library(DiagrammeR)
source("helper.R")

set.seed(101)

context("create_pmap_from_eventlog()")

test_that("create_pmap_from_eventlog() should handle simple graph", {
  eventlog <- data.frame(
     timestamp = c(as.POSIXct("2017-10-01"), as.POSIXct("2017-10-20")),
     customer_id = c("c1", "c1"),
     event_name = c("a", "b"),
     is_target = c(F, T),
     stringsAsFactors = FALSE
  )

  p <- create_pmap_from_eventlog(eventlog, render = F)

  ndf <- get_node_df(p)
  expect_equal(nrow(ndf), 2)

  edf <- get_edge_df(p)
  expect_equal(nrow(edf), 1)
})

test_that("create_pmap_from_eventlog() should handle complex graph", {
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

  # print("create_pmap()")
  p <- create_pmap_from_eventlog(eventlog, render = F)

  # print("render_graph()")
  expect_true(!any(is.null(render_graph(p))))
  # print(render_graph(p))
})
