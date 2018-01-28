context("apply_node_color()")

test_that("apply_node_color() handle graph without category", {
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
  # 'color' should be the default color
  expect_equal(node_df$color, rep("#01579B", nrow(node_df)))
  # 'fillcolor' should be the default gradient
  expect_equal(node_df$fillcolor, rep("#B3E5FC:#E1F5FE", nrow(node_df)))
})

test_that("apply_node_color() handle graph with a single category", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "campaign", "campaign", "campaign"),
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
  for (i in 1:nrow(node_df)) {
    # 'color' should not be the default color
    expect_true(node_df$color[i] != "#01579B")
    # 'fillcolor' should not be the default gradient
    expect_true(node_df$fillcolor[i] != "#B3E5FC:#E1F5FE")
  }
})

test_that("apply_node_color() handle graph with multiple categories", {
  p <- create_pmap_graph(
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      category = c("campaign", "campaign", "sale", "visit", "visit"),
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
  # Compare with the default color
  for (i in 1:nrow(node_df)) {
    # 'color' should not be the default color
    expect_true(node_df$color[i] != "#01579B")
    # 'fillcolor' should not be the default gradient
    expect_true(node_df$fillcolor[i] != "#B3E5FC:#E1F5FE")
  }

  # the 'color'/'fillcolor' of same category should be the same
  expect_true(node_df$color[1]      == node_df$color[2])
  expect_true(node_df$fillcolor[1]  == node_df$fillcolor[2])

  expect_true(node_df$color[4]      == node_df$color[5])
  expect_true(node_df$fillcolor[4]  == node_df$fillcolor[5])

  # the 'color'/'fillcolor' of different category should be different
  expect_true(node_df$color[1]      != node_df$color[3])
  expect_true(node_df$fillcolor[1]  != node_df$fillcolor[3])

  expect_true(node_df$color[1]      != node_df$color[4])
  expect_true(node_df$fillcolor[1]  != node_df$fillcolor[4])

  expect_true(node_df$color[3]      != node_df$color[5])
  expect_true(node_df$fillcolor[3]  != node_df$fillcolor[5])
})
