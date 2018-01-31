context("generate_eventlog()")

test_that("generate_eventlog() should be able to generate eventlog of given size", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 1000,
    number_of_cases = 10,
    categories = c("normal", "target"),
    categories_size = c(8, 2)
  )

  expect_equal(nrow(eventlog), 1000)
  expect_equal(nrow(eventlog %>% dplyr::distinct(case_id)), 10)
  expect_equal(nrow(eventlog %>% dplyr::distinct(activity)), 10)
  expect_equal(nrow(eventlog %>% dplyr::distinct(category)), 2)
  expect_equal(nrow(eventlog %>% dplyr::filter(category == "normal") %>% dplyr::distinct(activity)), 8)
  expect_equal(nrow(eventlog %>% dplyr::filter(category == "target") %>% dplyr::distinct(activity)), 2)
})
