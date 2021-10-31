context("generate_edges()")

test_that("generate_edges() should handle minimal eventlog", {
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-07-01"), as.POSIXct("2017-07-20")),
      case_id = c("c1", "c1"),
      activity = c("a", "b"),
      category = c("campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$amount, 1)

  # test duration
  expect_equal(edges$mean_duration, as.difftime(19, units = "days"))
  expect_equal(edges$median_duration, as.difftime(19, units = "days"))
  expect_equal(edges$max_duration, as.difftime(19, units = "days"))
  expect_equal(edges$min_duration, as.difftime(19, units = "days"))
})

test_that("generate_edges() should handle eventlog without specifies 'target_categories'", {
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-07-20"), as.POSIXct("2017-07-01")),
      case_id = c("c1", "c1"),
      activity = c("a", "b"),
      category = c("compaign", "sale"),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(nrow(edges), 1)

    # test duration
  expect_equal(edges$mean_duration, as.difftime(19, units = "days"))
  expect_equal(edges$median_duration, as.difftime(19, units = "days"))
  expect_equal(edges$max_duration, as.difftime(19, units = "days"))
  expect_equal(edges$min_duration, as.difftime(19, units = "days"))
})

test_that("generate_edges() should handle eventlog without edge reaches 'target_categories'", {
  # There is no case activity ends in `category == 'sale'`
  edges <- generate_edges(
    data.frame(
      timestamp = c(as.POSIXct("2017-07-20"), as.POSIXct("2017-07-01")),
      case_id = c("c1", "c1"),
      activity = c("a", "b"),
      category = c("compaign", "sale"),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  expect_equal(nrow(edges), 0)
})


test_that("generate_edges() should handle empty eventlog", {
  expect_equal(
    nrow(
      generate_edges(
        data.frame(), target_categories = c("sale")
      )
    ),
    0
  )
})

test_that("generate_edges() should count every paths if 'distinct_case' is not set", {
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
      case_id = c("c1", "c1", "c2", "c1", "c1", "c1", "c1"),
      activity = c("a", "b", "a", "a", "b", "a", "b"),
      category = c("campaign", "sale", "campaign", "campaign", "sale", "campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )

  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$amount, 3)

    # test duration
  expect_equal(edges$mean_duration, as.difftime(5, units = "days"))
  expect_equal(edges$median_duration, as.difftime(4, units = "days"))
  expect_equal(edges$max_duration, as.difftime(10, units = "days"))
  expect_equal(edges$min_duration, as.difftime(1, units = "days"))
})


test_that("generate_edges() should count unique 'case_id' if 'distinct_case' is set", {
  edges <- generate_edges(
    data.frame(
      timestamp = c(
        as.POSIXct("2017-07-01"),
        as.POSIXct("2017-07-02"),
        as.POSIXct("2017-07-03"),
        as.POSIXct("2017-07-04"),
        as.POSIXct("2017-07-20")
      ),
      case_id = c("c1", "c1", "c2", "c1", "c1"),
      activity = c("a", "b", "a", "a", "b"),
      category = c("campaign", "sale", "campaign", "campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    distinct_case = TRUE,
    target_categories = c("sale")
  )

  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$amount, 1)

  # test duration
  expect_equal(edges$mean_duration, as.difftime(8.5, units = "days"))
  expect_equal(edges$median_duration, as.difftime(8.5, units = "days"))
  expect_equal(edges$max_duration, as.difftime(16, units = "days"))
  expect_equal(edges$min_duration, as.difftime(1, units = "days"))
})

test_that("generate_edges() should not count paths from 'target_categories'", {
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
      case_id = c("c1", "c1", "c1", "c2", "c2", "c3", "c3"),
      activity = c("a", "b", "a", "b", "b", "a", "b"),
      category = c("campaign", "sale", "campaign", "sale", "sale", "campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )
  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$amount, 2)

  # test duration
  expect_equal(edges$mean_duration, as.difftime(7.5, units = "days"))
  expect_equal(edges$median_duration, as.difftime(7.5, units = "days"))
  expect_equal(edges$max_duration, as.difftime(14, units = "days"))
  expect_equal(edges$min_duration, as.difftime(1, units = "days"))
})

test_that("generate_edges() should convert 'timestamp' to 'POSIXct' if it's not 'POSIXct' yet", {
  edges <- generate_edges(
    data.frame(
      timestamp = c(
        "2017-07-01",
        "2017-07-02",
        "2017-07-03",
        "2017-07-04",
        "2017-07-05",
        "2017-07-06",
        "2017-07-20"
      ),
      case_id = c("c1", "c1", "c1", "c2", "c2", "c3", "c3"),
      activity = c("a", "b", "a", "b", "b", "a", "b"),
      category = c("campaign", "sale", "campaign", "sale", "sale", "campaign", "sale"),
      stringsAsFactors = FALSE
    ),
    target_categories = c("sale")
  )
  expect_equal(nrow(edges), 1)
  expect_equal(edges$from, "a")
  expect_equal(edges$to, "b")
  expect_equal(edges$amount, 2)

  # test duration
  expect_equal(edges$mean_duration, as.difftime(7.5, units = "days"))
  expect_equal(edges$median_duration, as.difftime(7.5, units = "days"))
  expect_equal(edges$max_duration, as.difftime(14, units = "days"))
  expect_equal(edges$min_duration, as.difftime(1, units = "days"))
})

test_that("generate_edges() should handle the eventlog without timestamp", {
  edges <- generate_edges(
    data.frame(
      case_id = c("c1", "c1", "c1", "c2", "c2", "c3", "c3"),
      activity = c("a", "b", "a", "b", "b", "a", "b"),
      category = c("campaign", "sale", "campaign", "sale", "sale", "campaign", "sale"),
      stringsAsFactors = FALSE
    )
  )
  expect_equal(nrow(edges), 3)
  expect_equal(edges$from, c("a", "b", "b"))
  expect_equal(edges$to, c("b", "a", "b"))
  expect_equal(edges$amount, c(2, 1, 1))

  # test duration
  expect_equal("mean_duration" %in% colnames(edges), FALSE)
  expect_equal("median_duration" %in% colnames(edges), FALSE)
  expect_equal("max_duration" %in% colnames(edges), FALSE)
  expect_equal("min_duration" %in% colnames(edges), FALSE)
})
