context("create_pmap_graph()")

test_that("create_pmap_graph() on basic graph", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 10000,
    number_of_cases = 1000,
    categories = c("campaign", "sale"),
    categories_size = c(10, 4)
  )

  expect_named(
    eventlog,
    c("timestamp", "case_id", "activity", "category"),
    ignore.order = TRUE,
    ignore.case = TRUE)
  expect_equal(nrow(eventlog), 10000)
  # print(str(eventlog))

  # print("generate_nodes()")
  nodes <- generate_nodes(eventlog)
  # print("generate_edges()")
  edges <- generate_edges(eventlog, target_categories = c("sale"))

  expect_true(all(c("from", "to", "amount", "mean_duration", "max_duration", "min_duration") %in% colnames(edges)))
  expect_gt(nrow(edges), 100)

  # print("> edges (generate_edges):")
  # print(str(edges))

  # print("create_pmap_graph()")
  p <- create_pmap_graph(nodes, edges, target_categories = c("sale"))

  edges_from_graph <- DiagrammeR::get_edge_df(p)
  # print("> edges_from_graph:")
  # print(str(edges_from_graph))

  expect_equal(nrow(edges), nrow(edges_from_graph))

  # check default label
  # print(str(edges_from_graph$label))
  # print(paste("sum():", sum(edges_from_graph$label == paste0("   ", edges$amount, "   "))))
  expect_equal(sum(edges_from_graph$label == paste0("   ", edges$amount, "   ")), nrow(edges))

  # print("render_graph()")
  expect_true(!any(is.null(render_pmap(p))))
  # print(render_graph(p))
})

test_that("create_pmap_graph() should assign `0` to `NA` in `inbound` and `outbound`", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "campaign", "sale", "sale"),
      amount = c(10, 30, 20, 40, NA),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 30, 20, 40),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  node_df <- DiagrammeR::get_node_df(p)
  expect_true(!any(is.na(node_df$inbound)))
  expect_true(!any(is.na(node_df$outbound)))
  expect_true(!any(is.na(node_df$amount)))
})


test_that("create_pmap_graph() should handle the edge without duration", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "campaign", "sale", "sale"),
      amount = c(10, 30, 20, 40, NA),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 30, 20, 40),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  # as no timestamp are given, so no `xxx_duration` should exist.
  edge_df <- DiagrammeR::get_edge_df(p)
  expect_equal("mean_duration" %in% colnames(edge_df), FALSE)
  expect_equal("median_duration" %in% colnames(edge_df), FALSE)
  expect_equal("max_duration" %in% colnames(edge_df), FALSE)
  expect_equal("min_duration" %in% colnames(edge_df), FALSE)

  expect_equal(edge_df$tooltip, c(
    "from: a\nto: b\namount: 10",
    "from: b\nto: c\namount: 30",
    "from: b\nto: d\namount: 20",
    "from: a\nto: e\namount: 40"
  ))
})