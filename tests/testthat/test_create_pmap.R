set.seed(101)

context("create_pmap()")

test_that("create_pmap() should handle simple graph", {
  eventlog <- data.frame(
     timestamp = c(as.POSIXct("2017-10-01"), as.POSIXct("2017-10-20")),
     customer_id = c("c1", "c1"),
     event_name = c("a", "b"),
     event_type = c("campaign", "sale"),
     stringsAsFactors = FALSE
  )

  p <- create_pmap(eventlog, target_types = c("sale"))

  ndf <- DiagrammeR::get_node_df(p)
  expect_equal(nrow(ndf), 2)

  edf <- DiagrammeR::get_edge_df(p)
  expect_equal(nrow(edf), 1)
})

test_that("create_pmap() should handle complex graph", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 10000,
    number_of_customers = 1000,
    event_catalogs = c("campaign", "sale"),
    event_catalogs_size = c(10, 4)
  )

  expect_named(
    eventlog,
    c("timestamp", "customer_id", "event_name", "event_type"),
    ignore.order = TRUE,
    ignore.case = TRUE)
  expect_equal(nrow(eventlog), 10000)

  # print(str(eventlog))
  # print("create_pmap_graph()")
  p <- create_pmap(eventlog, target_types = c("sale"))

  # print(generate_dot(p))

  # print("render_graph()")
  expect_true(!any(is.null(render_pmap(p))))
  print(render_pmap(p))
})
