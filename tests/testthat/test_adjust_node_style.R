context("adjust_node_style()")

test_that("adjust_node_style() handle graph without nodes's `amount`", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = NA,
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    )
  )

  node_df <- DiagrammeR::get_node_df(p)
  # 'fontsize' should be the default value `16`
  expect_equal(node_df$fontsize, rep(16, nrow(node_df)))
  # 'label' should be the default value, which is the `name` field
  expect_equal(node_df$label, node_df$name)
})

test_that("adjust_node_style() handle graph with node's amount", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "campaign", "campaign", "campaign"),
      amount = c(1, 5, 10, 15, 20),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "b", "b", "a"),
      to = c("b", "c", "d", "e"),
      amount = c(10, 20, 30, 40),
      stringsAsFactors = FALSE
    )
  )

  node_df <- DiagrammeR::get_node_df(p)
  projs <- projection(node_df$amount, 10, 20)
  labels <- paste0(node_df$name, "\n(", node_df$amount, ")")
  for (i in 1:nrow(node_df)) {
    # 'fontsize' should be the projected value
    expect_equal(node_df$fontsize[i], projs[i])
    # 'label' should not be the `name` field + `amount`
    expect_equal(node_df$label[i], labels[i])
  }
})
