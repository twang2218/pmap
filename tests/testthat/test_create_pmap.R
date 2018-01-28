set.seed(101)

context("create_pmap()")

test_that("create_pmap() should handle simple graph", {
  eventlog <- data.frame(
     timestamp = c(as.POSIXct("2017-10-01"), as.POSIXct("2017-10-20")),
     customer_id = c("c1", "c1"),
     event_name = c("a", "b"),
     event_category = c("campaign", "sale"),
     stringsAsFactors = FALSE
  )

  p <- create_pmap(eventlog, target_categories = c("sale"))

  ndf <- DiagrammeR::get_node_df(p)
  expect_equal(nrow(ndf), 2)

  edf <- DiagrammeR::get_edge_df(p)
  expect_equal(nrow(edf), 1)
})

test_that("create_pmap() should handle complex graph", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 10000,
    number_of_customers = 1000,
    event_categories = c("campaign", "sale"),
    event_categories_size = c(10, 4)
  )

  expect_named(
    eventlog,
    c("timestamp", "customer_id", "event_name", "event_category"),
    ignore.order = TRUE,
    ignore.case = TRUE)
  expect_equal(nrow(eventlog), 10000)

  # print(str(eventlog))
  # print("create_pmap_graph()")
  p <- create_pmap(eventlog, target_categories = c("sale"))

  # print(generate_dot(p))

  # print("render_graph()")
  expect_true(!any(is.null(render_pmap(p))))
  # print(render_pmap(p))
})

test_that("create_pmap() should handle more complex graph with multiple categories without target", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 10000,
    number_of_customers = 1000,
    event_categories = c("campaign", "visit", "phone", "sale"),
    event_categories_size = c(5, 3, 2, 4)
  )

  expect_named(
    eventlog,
    c("timestamp", "customer_id", "event_name", "event_category"),
    ignore.order = TRUE,
    ignore.case = TRUE)
  expect_equal(nrow(eventlog), 10000)

  # print(str(eventlog))
  # print("create_pmap_graph()")
  p <- eventlog %>% create_pmap() %>% prune_nodes(0.3) %>% prune_edges(0.3)

  # print(DiagrammeR::generate_dot(p))

  # print("render_graph()")
  expect_true(!any(is.null(render_pmap(p))))
  print(render_pmap(p))
})

test_that("create_pmap() should handle names with SPACE padding", {
  p <- create_pmap(
    data.frame(
      timestamp = c(
        "2017-07-01",
        "2017-07-02",
        "2017-07-03",
        "2017-07-04",
        "2017-07-05",
        "2017-07-06",
        "2017-07-20"
      ),
      customer_id = c("c1", "c1 ", "c1 ", "c2 ", "c2", "c3", "c3 "),
      event_name = c("  a", "b  ", "a  ", "b", " b", "  a", "b "),
      event_category = c("  campaign", " sale", "campaign  ", " sale  ", " sale", " campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    target_categories = c(" sale")
  )

  nodes <- DiagrammeR::get_node_df(p)
  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$type, c("campaign", "sale"))
  expect_equal(nodes$amount, c(3, 4))

  edges <- DiagrammeR::get_edge_df(p)
  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, 1)
  expect_equal(edges$to, 2)
  expect_equal(edges$amount, 2)

  # test duration
  expect_equal(edges$mean_duration, "1.07 weeks")
  expect_equal(edges$median_duration, "1.07 weeks")
  expect_equal(edges$max_duration, "2 weeks")
  expect_equal(edges$min_duration, "1 days")
})
