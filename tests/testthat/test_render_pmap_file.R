context("render_pmap_file()")

test_that("render_pmap_file() should be able to render to a file", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 10000,
    number_of_customers = 1000,
    event_catalogs = c("campaign", "sale"),
    event_catalogs_size = c(10, 4)
  )

  p <- eventlog %>% create_pmap() %>% prune_nodes(0.5) %>% prune_edges(0.5)

  for (format in c("pdf", "svg", "png", "ps")) {
    file_name <- tempfile(fileext = paste0(".", format))
    render_pmap_file(p, file_name, format = format)
    expect_true(file.exists(file_name))
  }
})
