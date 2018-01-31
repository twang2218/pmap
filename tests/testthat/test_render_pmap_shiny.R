context("render_pmap_shiny()")

test_that("render_pmap_shiny() should be able to show the web app", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 10000,
    number_of_cases = 1000,
    categories = c("campaign", "sale"),
    categories_size = c(10, 4)
  )

  app <- eventlog %>% create_pmap() %>% render_pmap_shiny()
  expect_equal(class(app), "shiny.appobj")
  expect_equal(length(app), 5)
})
