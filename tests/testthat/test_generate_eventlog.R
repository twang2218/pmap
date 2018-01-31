context("generate_eventlog()")

test_that("generate_eventlog() should be able to generate eventlog of given size", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 1000,
    number_of_cases = 10,
    event_categories = c("normal", "target"),
    event_categories_size = c(8, 2)
  )

  expect_equal(nrow(eventlog), 1000)
  expect_equal(nrow(eventlog %>% dplyr::distinct(case_id)), 10)
  expect_equal(nrow(eventlog %>% dplyr::distinct(event_name)), 10)
  expect_equal(nrow(eventlog %>% dplyr::distinct(event_category)), 2)
  expect_equal(nrow(eventlog %>% dplyr::filter(event_category == "normal") %>% dplyr::distinct(event_name)), 8)
  expect_equal(nrow(eventlog %>% dplyr::filter(event_category == "target") %>% dplyr::distinct(event_name)), 2)
})
