library(pmap)

context("generate_nodes()")

test_that("generate_nodes() should handle minimal eventlog", {
  nodes <- generate_nodes(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-20")
      ),
      customer_id = c("c1", "c1"),
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
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-02"),
        as.POSIXct("2017-10-03"),
        as.POSIXct("2017-10-04"),
        as.POSIXct("2017-10-20")
      ),
      customer_id = c("c1", "c1", "c2", "c1", "c1"),
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
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-02"),
        as.POSIXct("2017-10-03"),
        as.POSIXct("2017-10-04"),
        as.POSIXct("2017-10-20")
      ),
      customer_id = c("c1", "c1", "c2", "c1", "c1"),
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

test_that("generate_nodes() should count unique 'customer_id' if 'distinct_customer' is set", {
  nodes <- generate_nodes(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-02"),
        as.POSIXct("2017-10-03"),
        as.POSIXct("2017-10-04"),
        as.POSIXct("2017-10-20")
      ),
      customer_id = c("c1", "c1", "c2", "c1", "c1"),
      event_name = c("a", "b", "a", "a", "b"),
      is_target = c(F, T, F, F, T),
      stringsAsFactors = FALSE
    ),
    distinct_customer = T
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$amount, c(2, 1))
})

test_that("generate_nodes() should count every path if 'distinct_customer' is not set", {
  nodes <- generate_nodes(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-02"),
        as.POSIXct("2017-10-03"),
        as.POSIXct("2017-10-04"),
        as.POSIXct("2017-10-20")
      ),
      customer_id = c("c1", "c1", "c2", "c1", "c1"),
      event_name = c("a", "b", "a", "a", "b"),
      is_target = c(F, T, F, F, T),
      stringsAsFactors = FALSE
    ),
    distinct_customer = F
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$amount, c(3, 2))
})
