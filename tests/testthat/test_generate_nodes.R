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
      event_type = c("campaign", "sale"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$type, c("campaign", "sale"))
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
      event_type = c("sale", "campaign", "sale", "campaign", "campaign"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$type, c("sale", "campaign"))
})

test_that("generate_nodes() should handle eventlog with space at beginning or end of the 'event_name'", {
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
      event_type = c("sale", "campaign", "sale", "campaign", "campaign"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b b"))
  expect_equal(nodes$type, c("sale", "campaign"))
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
      event_type = c("campaign", "sale", "campaign", "campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    distinct_customer = TRUE
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$amount, c(2, 1))
  expect_equal(nodes$type, c("campaign", "sale"))
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
      event_type = c("campaign", "sale", "campaign", "campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    distinct_customer = FALSE
  )

  expect_equal(nrow(nodes), 2)
  expect_equal(nodes$name, c("a", "b"))
  expect_equal(nodes$amount, c(3, 2))
  expect_equal(nodes$type, c("campaign", "sale"))
})
