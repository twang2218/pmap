context("generate_edges()")

test_that("generate_edges() should handle minimal eventlog", {
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-10-01"), as.POSIXct("2017-10-20")),
      customer_id = c("c1", "c1"),
      event_name = c("a", "b"),
      event_type = c("campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    target_types = c("sale")
  )

  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$amount, 1)
})

test_that("generate_edges() should handle eventlog without specifies 'target_types'", {
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-10-20"), as.POSIXct("2017-10-01")),
      customer_id = c("c1", "c1"),
      event_name = c("a", "b"),
      event_type = c("compaign", "sale"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(edges), 1)
})

test_that("generate_edges() should handle eventlog without edge reaches 'target_types'", {
  # There is no customer event ends in `event_type == 'sale'`
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-10-20"), as.POSIXct("2017-10-01")),
      customer_id = c("c1", "c1"),
      event_name = c("a", "b"),
      event_type = c("compaign", "sale"),
      stringsAsFactors = FALSE
    ),
    target_types = c("sale")
  )

  expect_equal(nrow(edges), 0)
})


test_that("generate_edges() should handle empty eventlog", {
  expect_equal(
    nrow(
      generate_edges(
        data.frame(), target_types = c("sale")
      )
    ),
    0
  )
})

test_that("generate_edges() should count every paths if 'distinct_customer' is not set", {
  edges <- generate_edges(
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
    target_types = c("sale")
  )

  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$amount, 2)
})


test_that("generate_edges() should count unique 'customer_id' if 'distinct_customer' is set", {
  edges <- generate_edges(
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
    distinct_customer = TRUE,
    target_types = c("sale")
  )

  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$amount, 1)
})

test_that("generate_edges() should not count paths from 'target_types'", {
  # browser()
  edges <- generate_edges(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-10-01"),
        as.POSIXct("2017-10-02"),
        as.POSIXct("2017-10-03"),
        as.POSIXct("2017-10-04"),
        as.POSIXct("2017-10-05"),
        as.POSIXct("2017-10-06"),
        as.POSIXct("2017-10-20")
      ),
      customer_id = c("c1", "c1", "c1", "c2", "c2", "c3", "c3"),
      event_name = c("a", "b", "a", "b", "b", "a", "b"),
      event_type = c("campaign", "sale", "campaign", "sale", "sale", "campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    target_types = c("sale")
  )
  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$amount, 2)
})
