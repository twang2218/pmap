library(data.table)

context("get_attrs_desc()")

test_that("get_attrs_desc() should handle 'list'", {
  expect_equal(
    get_attrs_desc(
      list(id = 1, name = "Elena", is_manager = FALSE)
    ),
    "id: 1\nname: Elena\nis_manager: FALSE"
  )

  expect_equal(get_attrs_desc(list()), "")
})

test_that("get_attrs_desc() should handle 'data.frame'", {
  expect_equal(
    get_attrs_desc(
      data.frame(
        id = c(1, 2, 3, 4),
        name = c("Jane", "John", "Eric", "Selena"),
        is_manager = c(FALSE, FALSE, FALSE, TRUE)
      )
    ),
    c(
      "id: 1\nname: Jane\nis_manager: FALSE",
      "id: 2\nname: John\nis_manager: FALSE",
      "id: 3\nname: Eric\nis_manager: FALSE",
      "id: 4\nname: Selena\nis_manager: TRUE"
    )
  )

  expect_equal(get_attrs_desc(data.frame()), "")
})

test_that("get_attrs_desc() should handle minimal 'data.frame'", {
  expect_equal(
    get_attrs_desc(
      data.frame(
        id = c(1),
        name = c("Jane"),
        is_manager = c(FALSE)
      )
    ),
    "id: 1\nname: Jane\nis_manager: FALSE"
  )

  expect_equal(get_attrs_desc(data.frame()), "")
})

test_that("get_attrs_desc() should handle 'matrix'", {
  expect_equal(
    get_attrs_desc(
      matrix(1:9, nrow = 3, dimnames = list(NULL, c("id", "x", "y")))
    ),
    c(
      "id: 1\nx: 4\ny: 7",
      "id: 2\nx: 5\ny: 8",
      "id: 3\nx: 6\ny: 9"
    )
  )

  expect_equal(get_attrs_desc(matrix), "")
})

test_that("get_attrs_desc() should handle inherit type", {
  expect_equal(
    get_attrs_desc(
      data.table(
        id = c(1, 2, 3, 4),
        name = c("Jane", "John", "Eric", "Selena"),
        is_manager = c(FALSE, FALSE, FALSE, TRUE)
      )
    ),
    c(
      "id: 1\nname: Jane\nis_manager: FALSE",
      "id: 2\nname: John\nis_manager: FALSE",
      "id: 3\nname: Eric\nis_manager: FALSE",
      "id: 4\nname: Selena\nis_manager: TRUE"
    )
  )

  expect_equal(get_attrs_desc(data.table()), "")
})