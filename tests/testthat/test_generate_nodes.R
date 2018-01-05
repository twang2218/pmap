library(pmap)

context("generate_nodes()")

test_that("generate_nodes() should handle minimal eventlog", {
  nodes <- generate_nodes(
    data.frame(
      event_name = c("a", "b"),
      is_target = c(T, F),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$is_target, c(T, F))
})

test_that("generate_nodes() should handle eventlog with duplicated events", {
  nodes <- generate_nodes(
    data.frame(
      event_name = c("a", "b", "a", "b", "b"),
      is_target = c(T, F, T, F, F),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$is_target, c(T, F))
})

test_that("generate_nodes() should handle eventlog with space at begining or end of the 'event_name'", {
  nodes <- generate_nodes(
    data.frame(
      event_name = c(" a", "b b ", " a ", " b b ", " b b"),
      is_target = c(T, F, T, F, F),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b b"))
  expect_equal(nodes$is_target, c(T, F))
})

test_that("generate_nodes() should handle empty eventlog", {
  expect_equal(nrow(generate_nodes(data.frame())), 0)
})
