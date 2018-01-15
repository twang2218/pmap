context("utils.R")

test_that("projection()", {
  expect_equal(projection(1, 10, 20), 15)
  expect_equal(projection(c(0, 0, 0), 10, 20), 15)
  # expect_equal(projection(NULL, 10, 20), 15)
  # expect_equal(projection(NA, 10, 20), 15)
  expect_equal(projection(c(1, 2), 10, 20), c(10, 20))
  expect_equal(projection(c(0, 5, 10), 10, 20), c(10, 15, 20))
})

test_that("get_palette()", {
  expect_equal(length(get_palette(1)), 1)
  expect_equal(length(get_palette(5)), 5)
  expect_equal(length(get_palette(19)), 19)
  # only can get 19 colors, no more
  expect_equal(length(get_palette(50)), 19)

  expect_equal(names(get_palette(4)), c("blue", "red", "green", "yellow"))
  expect_equal(
    get_palette(4),
    c(
      blue = "#0C46A0FF",
      red = "#B71B1BFF",
      green = "#1A5E1FFF",
      yellow = "#F47F17FF"
    )
  )
})

test_that("get_color_variants()", {
  expect_equal(get_color_variants("#123456"), c("#1234564C", "#12345680", "#123456FF"))
})

test_that("get_colors()", {
  expect_equal(
    get_colors(c("type1")),
    list(type1 = c("#0C46A04C", "#0C46A080", "#0C46A0FF"))
  )
  expect_equal(
    get_colors(c("a", "b", "c")),
    list(
      a = c("#0C46A04C", "#0C46A080", "#0C46A0FF"),
      b = c("#B71B1B4C", "#B71B1B80", "#B71B1BFF"),
      c = c("#1A5E1F4C", "#1A5E1F80", "#1A5E1FFF")
    )
  )

  # return empty list for invalid input
  expect_equal(get_colors(c()), list())
  expect_equal(get_colors(NULL), list())
})

test_that("generate_random_datetimes()", {
  dates <- generate_random_datetimes(10)
  expect_is(dates, "POSIXct")
  expect_equal(length(dates), 10)
})
