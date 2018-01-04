library(pmap)

source("helper.R")

context("generate_edges()")

test_that("generate_edges() should handle minimal eventlog", {
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-10-01"), as.POSIXct("2017-10-20")),
      customer_id = c("c1", "c1"),
      event_name = c("a", "b"),
      is_target = c(F, T),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$value, 1)
})

test_that("generate_edges() should handle eventlog without edge", {
  # There is no customer event ends in `is_target == T`
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-10-20"), as.POSIXct("2017-10-01")),
      customer_id = c("c1", "c1"),
      event_name = c("a", "b"),
      is_target = c(F, T),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(edges), 0)
})


test_that("generate_edges() should handle empty eventlog", {
  expect_equal(
    nrow(
      generate_edges(
        data.frame()
      )
    ),
    0
  )
})
