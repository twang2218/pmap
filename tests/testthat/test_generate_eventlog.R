library(dplyr)

context("generate_eventlog()")

test_that("generate_eventlog() should be able to generate eventlog of given size", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 1000,
    number_of_customers = 10,
    event_catalogs = c("normal", "target"),
    event_catalogs_size = c(8, 2)
  )

  expect_equal(nrow(eventlog), 1000)
  expect_equal(nrow(eventlog %>% distinct(customer_id)), 10)
  expect_equal(nrow(eventlog %>% distinct(event_name)), 10)
  expect_equal(nrow(eventlog %>% distinct(event_type)), 2)
  expect_equal(nrow(eventlog %>% filter(event_type == "normal") %>% distinct(event_name)), 8)
  expect_equal(nrow(eventlog %>% filter(event_type == "target") %>% distinct(event_name)), 2)
})
