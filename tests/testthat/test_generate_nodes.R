context("generate_nodes()")

test_that("generate_nodes() should handle minimal eventlog", {
  nodes <- generate_nodes(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-20")
      ),
      case_id = c("c1", "c1"),
      activity = c("a", "b"),
      category = c("campaign", "sale"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$category, c("campaign", "sale"))
})

test_that("generate_nodes() should handle eventlog with duplicated activities", {
  nodes <- generate_nodes(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-02"),
        as.POSIXct("2017-10-03"),
        as.POSIXct("2017-10-04"),
        as.POSIXct("2017-10-20")
      ),
      case_id = c("c1", "c1", "c2", "c1", "c1"),
      activity = c("a", "b", "a", "b", "b"),
      category = c("sale", "campaign", "sale", "campaign", "campaign"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$category, c("sale", "campaign"))
})

test_that("generate_nodes() should handle eventlog with space at beginning or end of the 'activity'", {
  nodes <- generate_nodes(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-02"),
        as.POSIXct("2017-10-03"),
        as.POSIXct("2017-10-04"),
        as.POSIXct("2017-10-20")
      ),
      case_id = c("c1", "c1", "c2", "c1", "c1"),
      activity = c(" a", "b b ", " a ", " b b ", " b b"),
      category = c("sale", "campaign", "sale", "campaign", "campaign"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b b"))
  expect_equal(nodes$category, c("sale", "campaign"))
})

test_that("generate_nodes() should handle empty eventlog", {
  expect_equal(nrow(generate_nodes(data.frame())), 0)
})

test_that("generate_nodes() should count unique 'case_id' if 'distinct_case' is set", {
  nodes <- generate_nodes(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-02"),
        as.POSIXct("2017-10-03"),
        as.POSIXct("2017-10-04"),
        as.POSIXct("2017-10-20")
      ),
      case_id = c("c1", "c1", "c2", "c1", "c1"),
      activity = c("a", "b", "a", "a", "b"),
      category = c("campaign", "sale", "campaign", "campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    distinct_case = TRUE
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$amount, c(2, 1))
  expect_equal(nodes$category, c("campaign", "sale"))
})

test_that("generate_nodes() should count every path if 'distinct_case' is not set", {
  nodes <- generate_nodes(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-02"),
        as.POSIXct("2017-10-03"),
        as.POSIXct("2017-10-04"),
        as.POSIXct("2017-10-20")
      ),
      case_id = c("c1", "c1", "c2", "c1", "c1"),
      activity = c("a", "b", "a", "a", "b"),
      category = c("campaign", "sale", "campaign", "campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    distinct_case = FALSE
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$amount, c(3, 2))
  expect_equal(nodes$category, c("campaign", "sale"))
})

test_that("generate_nodes() should handle the eventlog without `category`", {
  nodes <- generate_nodes(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-02"),
        as.POSIXct("2017-10-03"),
        as.POSIXct("2017-10-04"),
        as.POSIXct("2017-10-20")
      ),
      case_id = c("c1", "c1", "c2", "c1", "c1"),
      activity = c("a", "b", "a", "a", "b"),
      stringsAsFactors = FALSE
    ),
    distinct_case = TRUE
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$amount, c(2, 1))
  expect_equal(nodes$category, c("a", "b"))
})
