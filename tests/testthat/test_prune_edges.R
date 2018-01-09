context("prune_edges()")

test_that("prune_edges() should be able prune nothing", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      type = c("campaign", "campaign", "campaign", "sale", "sale"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    ),
    target_types = c("sale")
  )

  edges_count_before_prune <- nrow(get_edge_df(p))

  # 0.1 * 4 = 0.4 ~= 0, so `prune_edges()` should prune nothing
  p <- prune_edges(p, percentage = 0.1)

  edges_count_after_prune <- nrow(get_edge_df(p))

  expect_equal(edges_count_after_prune, edges_count_before_prune)
})

test_that("prune_edges() should be able prune half of the edges", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      type = c("campaign", "campaign", "campaign", "sale", "sale"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    ),
    target_types = c("sale")
  )

  edges_count_before_prune <- nrow(get_edge_df(p))

  # 0.5 * 4 = 2, so `prune_edges()` should prune 2 edges,
  # which should be "a => b", "b => c" according to the `amount`
  p <- prune_edges(p, percentage = 0.5)

  edges_count_after_prune <- nrow(get_edge_df(p))

  expect_equal(edges_count_after_prune, edges_count_before_prune / 2)
})


test_that("prune_edges() should be able prune all of the edges", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      type = c("campaign", "campaign", "campaign", "sale", "sale"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    ),
    target_types = c("sale")
  )

  edges_count_before_prune <- nrow(get_edge_df(p))

  # 1 * 4 = 4, so `prune_edges()` should prune all 4 edges
  p <- prune_edges(p, percentage = 1)

  edges_count_after_prune <- nrow(get_edge_df(p))

  expect_equal(edges_count_after_prune, 0)
})
