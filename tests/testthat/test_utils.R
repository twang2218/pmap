context("utils.R")

test_that("projection()", {
  expect_equal(projection(1, 10, 20), 15)
  expect_equal(projection(c(0, 0, 0), 10, 20), 15)
  # expect_equal(projection(NULL, 10, 20), 15)
  # expect_equal(projection(NA, 10, 20), 15)
  expect_equal(projection(c(1, 2), 10, 20), c(10, 20))
  expect_equal(projection(c(0, 5, 10), 10, 20), c(10, 15, 20))
})

test_that("get_color_variant()", {
  expect_equal(get_color_variant("#0C46A0", 1), "#0C46A0")
  expect_equal(get_color_variant("#0C46A0", 0.5), "#85A2CF")
  expect_equal(get_color_variant("#0C46A0", 0.3), "#B6C7E2")
  expect_equal(get_color_variant("#B71C1CFF", 1), "#B71C1C")
  expect_equal(get_color_variant("#B71C1CFF", 0.3), "#E9BABA")
})

test_that("get_colors()", {
  expect_equal(nrow(get_colors(c())), 0)
  expect_equal(nrow(get_colors(NULL)), 0)

  #        type   color       fillcolor
  # blue normal #0D47A1 #86A3D0:#B6C7E2
  pal <- get_colors(c("normal"))
  expect_equal(pal[1,"type"], "normal")
  expect_equal(pal[1,"color"], "#0D47A1")
  expect_equal(pal[1,"fillcolor"], "#86A3D0:#B6C7E2")

  #        type   color       fillcolor
  # blue normal #0D47A1 #86A3D0:#B6C7E2
  # red  target #B71C1C #DB8D8D:#E9BABA
  pal <- get_colors(c("normal", "target"))
  expect_equal(pal[1,"type"], "normal")
  expect_equal(pal[1,"color"], "#0D47A1")
  expect_equal(pal[1,"fillcolor"], "#86A3D0:#B6C7E2")
  expect_equal(pal[2,"type"], "target")
  expect_equal(pal[2,"color"], "#B71C1C")
  expect_equal(pal[2,"fillcolor"], "#DB8D8D:#E9BABA")
})

test_that("generate_random_datetimes()", {
  dates <- generate_random_datetimes(10)
  expect_is(dates, "POSIXct")
  expect_equal(length(dates), 10)
})

test_that("format_duration()", {
  dbl2diff <- function(d) {
    as.difftime(d, units = "secs")
  }

  # format_duration() should be able to format the duration to human friendly format.
  expect_equal(format_duration(dbl2diff(1161.2380952381)), "19.35 minutes")
  expect_equal(format_duration(dbl2diff(51764.2222222222)), "14.38 hours")
  expect_equal(format_duration(dbl2diff(171633.037974684)), "1.99 days")
  # format_duration() should be able to handle vector
  expect_equal(format_duration(
      c(
        dbl2diff(1161.2380952381),
        dbl2diff(171633.037974684),
        dbl2diff(51764.2222222222)
      )
    ),
    c(
      "19.35 minutes",
      "1.99 days",
      "14.38 hours"
    )
  )
})
