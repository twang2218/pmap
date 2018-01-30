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

  #        category   color       fillcolor
  # blue normal #0D47A1 #86A3D0
  pal <- get_colors(c("normal"))
  expect_equal(pal[1, "category"], "normal")
  expect_equal(pal[1, "color"], "#0D47A1")
  expect_equal(pal[1, "fillcolor"], "#86A3D0")

  #        category   color       fillcolor
  # blue normal #0D47A1 #86A3D0
  # red  target #B71C1C #DB8D8D
  pal <- get_colors(c("normal", "target"))
  expect_equal(pal[1, "category"], "normal")
  expect_equal(pal[1, "color"], "#0D47A1")
  expect_equal(pal[1, "fillcolor"], "#86A3D0")
  expect_equal(pal[2, "category"], "target")
  expect_equal(pal[2, "color"], "#B71C1C")
  expect_equal(pal[2, "fillcolor"], "#DB8D8D")

  pal <- get_colors(paste0("color_", 1:100))
  expect_equal(nrow(pal), 100)
  # Material Design Palette
  expect_equal(pal[1, "color"], "#0D47A1")
  expect_equal(pal[2, "color"], "#B71C1C")
  expect_equal(pal[18, "color"], "#263238")
  expect_equal(pal[19, "color"], "#01579B")
  # IGV Palette
  expect_equal(pal[20, "color"], "#5050FF")
  expect_equal(pal[21, "color"], "#CE3D32")
  expect_equal(pal[44, "color"], "#A9A9A9")
  expect_equal(pal[45, "color"], "#33CC00")
  # NPG Palette
  expect_equal(pal[46, "color"], "#E64B35")
  expect_equal(pal[47, "color"], "#4DBBD5")
  expect_equal(pal[54, "color"], "#7E6148")
  expect_equal(pal[55, "color"], "#B09C85")
  # LocusZoom Palette
  expect_equal(pal[56, "color"], "#D43F3A")
  expect_equal(pal[57, "color"], "#EEA236")
  expect_equal(pal[61, "color"], "#9632B8")
  expect_equal(pal[62, "color"], "#B8B8B8")
  # Jama Palette
  expect_equal(pal[63, "color"], "#374E55")
  expect_equal(pal[64, "color"], "#DF8F44")
  expect_equal(pal[68, "color"], "#6A6599")
  expect_equal(pal[69, "color"], "#80796B")
  # Futurama Palette
  expect_equal(pal[70, "color"], "#C71000")
  expect_equal(pal[71, "color"], "#008EA0")
  expect_equal(pal[79, "color"], "#1A5354")
  expect_equal(pal[80, "color"], "#3F4041")
  # Out of palette capability
  expect_equal(pal[81, "color"], "#428BCA")
  expect_equal(pal[82, "color"], "#428BCA")
  expect_equal(pal[100, "color"], "#428BCA")
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

test_that("get_file_type()", {
  expect_equal(get_file_type("abc.def"), "def")
  expect_equal(get_file_type("/a/1/3.34/abc.DeF"), "def")
  expect_equal(get_file_type("/a/b/c/de.cd.ed.def"), "def")
  expect_equal(get_file_type("abc"), "")
  expect_equal(get_file_type("cdedf."), "")
})
