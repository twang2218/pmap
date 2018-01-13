context("utils.R")

test_that("projection() should be working correctly", {
  expect_equal(projection(1, 10, 20), 15)
  expect_equal(projection(c(0, 0, 0), 10, 20), 15)
  # expect_equal(projection(NULL, 10, 20), 15)
  # expect_equal(projection(NA, 10, 20), 15)
  expect_equal(projection(c(1, 2), 10, 20), c(10, 20))
  expect_equal(projection(c(0, 5, 10), 10, 20), c(10, 15, 20))
})