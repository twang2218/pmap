context("prune_nodes()")

test_that("prune_nodes() should be able prune nothing", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      type = c("campaign", "campaign", "campaign", "sale", "sale"),
      amount = c(10, 30, 20, 40, 5),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 30, 20, 40),
      stringsAsFactors = FALSE
    ),
    target_types = c("sale")
  )

  for (rank in c("amount", "in_degree", "out_degree")) {
    nodes_count_before_prune <- nrow(get_node_df(p))

    # 0.1 * 4 = 0.4 ~= 0, so `prune_nodes()` should prune nothing
    p1 <- prune_nodes(p, percentage = 0.1, rank = rank)

    nodes_count_after_prune <- nrow(get_node_df(p1))
    expect_equal(nodes_count_after_prune, nodes_count_before_prune)
  }

})

test_that("prune_nodes() should be able prune half of the nodes", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      type = c("campaign", "campaign", "campaign", "sale", "sale"),
      amount = c(10, 30, 20, 40, 5),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 30, 20, 40),
      stringsAsFactors = FALSE
    ),
    target_types = c("sale")
  )

  for (rank in c("amount", "in_degree", "out_degree")) {
    nodes_count_before_prune <- nrow(get_node_df(p))

    # 0.5 * 4 = 2, so `prune_nodes()` should prune 2 nodes,
    # which should be "a => b", "b => c" according to the `amount`
    p <- prune_nodes(p, percentage = 0.5, rank = rank)

    nodes_count_after_prune <- nrow(get_node_df(p))
    expect_equal(nodes_count_after_prune, ceiling(nodes_count_before_prune / 2))
  }
})


test_that("prune_nodes() should be able prune all of the nodes", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      type = c("campaign", "campaign", "campaign", "sale", "sale"),
      amount = c(10, 30, 20, 40, 5),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 30, 20, 40),
      stringsAsFactors = FALSE
    ),
    target_types = c("sale")
  )

  for (rank in c("amount", "in_degree", "out_degree")) {
    nodes_count_before_prune <- nrow(get_node_df(p))

    # 1 * 4 = 4, so `prune_nodes()` should prune all 4 nodes
    p <- prune_nodes(p, percentage = 1, rank = rank)

    nodes_count_after_prune <- nrow(get_node_df(p))
    expect_equal(nodes_count_after_prune, 0)
  }
})
