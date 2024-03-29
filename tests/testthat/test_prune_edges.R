context("prune_edges()")

test_that("prune_edges(percentage) should be able prune nothing", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "campaign", "sale", "sale"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  edges_count_before_prune <- nrow(DiagrammeR::get_edge_df(p))

  # round(0.1 * 4) = round(0.4) = 0, so `prune_edges()` should prune nothing
  p <- prune_edges(p, percentage = 0.1)

  edges_count_after_prune <- nrow(DiagrammeR::get_edge_df(p))

  expect_equal(edges_count_after_prune, edges_count_before_prune)
})

test_that("prune_edges(percentage) should be able prune half of the edges", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "campaign", "sale", "sale"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  edges_count_before_prune <- nrow(DiagrammeR::get_edge_df(p))

  # 0.5 * 4 = 2, so `prune_edges()` should prune 2 edges,
  # which should be "a => b", "b => c" according to the `amount`
  p <- prune_edges(p, percentage = 0.5)

  edges_count_after_prune <- nrow(DiagrammeR::get_edge_df(p))

  expect_equal(edges_count_after_prune, edges_count_before_prune - round(edges_count_before_prune * 0.5))
})


test_that("prune_edges(percentage) should be able prune all of the edges", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "campaign", "sale", "sale"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  edges_count_before_prune <- nrow(DiagrammeR::get_edge_df(p))

  # 1 * 4 = 4, so `prune_edges()` should prune all 4 edges
  p <- prune_edges(p, percentage = 1)

  edges_count_after_prune <- nrow(DiagrammeR::get_edge_df(p))

  expect_equal(edges_count_after_prune, 0)
})


test_that("prune_edges(max) should be able prune nothing", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "campaign", "sale", "sale"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  edges_count_before_prune <- nrow(DiagrammeR::get_edge_df(p))

  # total edges is 4, max is 4 as well, so nothing to prune
  p <- prune_edges(p, max = 4)

  edges_count_after_prune <- nrow(DiagrammeR::get_edge_df(p))

  expect_equal(edges_count_after_prune, edges_count_before_prune)
})

test_that("prune_edges(max) should be able prune half of the edges", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "campaign", "sale", "sale"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  edges_count_before_prune <- nrow(DiagrammeR::get_edge_df(p))

  # total 4 edges, max is 2, so 2 edges should be pruned
  p <- prune_edges(p, max = 2)

  edges_count_after_prune <- nrow(DiagrammeR::get_edge_df(p))

  expect_equal(edges_count_after_prune, 2)
})


test_that("prune_edges(max) should be able prune all of the edges", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "campaign", "sale", "sale"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  edges_count_before_prune <- nrow(DiagrammeR::get_edge_df(p))

  # total 4 edges, `max` is 0, so all 4 edges should be pruned
  p <- prune_edges(p, max = 0)

  edges_count_after_prune <- nrow(DiagrammeR::get_edge_df(p))

  expect_equal(edges_count_after_prune, 0)
})
