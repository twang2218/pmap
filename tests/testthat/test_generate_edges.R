context("generate_edges()")

test_that("generate_edges() should handle minimal eventlog", {
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-07-01"), as.POSIXct("2017-07-20")),
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

  # test duration
  expect_equal(edges$mean_duration, "2.71 weeks")
  expect_equal(edges$median_duration, "2.71 weeks")
  expect_equal(edges$max_duration, "2.71 weeks")
  expect_equal(edges$min_duration, "2.71 weeks")
})

test_that("generate_edges() should handle eventlog without specifies 'target_types'", {
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-07-20"), as.POSIXct("2017-07-01")),
      customer_id = c("c1", "c1"),
      event_name = c("a", "b"),
      event_type = c("compaign", "sale"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(edges), 1)

    # test duration
  expect_equal(edges$mean_duration, "2.71 weeks")
  expect_equal(edges$median_duration, "2.71 weeks")
  expect_equal(edges$max_duration, "2.71 weeks")
  expect_equal(edges$min_duration, "2.71 weeks")
})

test_that("generate_edges() should handle eventlog without edge reaches 'target_types'", {
  # There is no customer event ends in `event_type == 'sale'`
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-07-20"), as.POSIXct("2017-07-01")),
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
        as.POSIXct("2017-07-01"),
        as.POSIXct("2017-07-02"),
        as.POSIXct("2017-07-03"),
        as.POSIXct("2017-07-04"),
        as.POSIXct("2017-07-08"),
        as.POSIXct("2017-07-10"),
        as.POSIXct("2017-07-20")
      ),
      customer_id = c("c1", "c1", "c2", "c1", "c1", "c1", "c1"),
      event_name = c("a", "b", "a", "a", "b", "a", "b"),
      event_type = c("campaign", "sale", "campaign", "campaign", "sale", "campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    target_types = c("sale")
  )

  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$amount, 3)

    # test duration
  expect_equal(edges$mean_duration, "5 days")
  expect_equal(edges$median_duration, "4 days")
  expect_equal(edges$max_duration, "1.43 weeks")
  expect_equal(edges$min_duration, "1 days")
})


test_that("generate_edges() should count unique 'customer_id' if 'distinct_customer' is set", {
  edges <- generate_edges(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-07-01"),
        as.POSIXct("2017-07-02"),
        as.POSIXct("2017-07-03"),
        as.POSIXct("2017-07-04"),
        as.POSIXct("2017-07-20")
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

  # test duration
  expect_equal(edges$mean_duration, "1.21 weeks")
  expect_equal(edges$median_duration, "1.21 weeks")
  expect_equal(edges$max_duration, "2.29 weeks")
  expect_equal(edges$min_duration, "1 days")
})

test_that("generate_edges() should not count paths from 'target_types'", {
  # browser()
  edges <- generate_edges(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-07-01"),
        as.POSIXct("2017-07-02"),
        as.POSIXct("2017-07-03"),
        as.POSIXct("2017-07-04"),
        as.POSIXct("2017-07-05"),
        as.POSIXct("2017-07-06"),
        as.POSIXct("2017-07-20")
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

  # test duration
  expect_equal(edges$mean_duration, "1.07 weeks")
  expect_equal(edges$median_duration, "1.07 weeks")
  expect_equal(edges$max_duration, "2 weeks")
  expect_equal(edges$min_duration, "1 days")
})
