context("create_pmap_graph()")

test_that("create_pmap_graph()", {
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

  # print("generate_nodes()")
  nodes <- generate_nodes(eventlog)
  # print("generate_edges()")
  edges <- generate_edges(eventlog, target_types = c("sale"))

  expect_named(edges, c("from", "to", "amount"))
  expect_gt(nrow(edges), 100)

  # print(str(edges))

  # print("create_pmap_graph()")
  p <- create_pmap_graph(nodes, edges, target_types = c("sale"))

  edges_from_graph <- DiagrammeR::get_edge_df(p)
  expect_equal(nrow(edges), nrow(edges_from_graph))

  # print("render_graph()")
  expect_true(!any(is.null(render_pmap(p))))
  # print(render_graph(p))
})
