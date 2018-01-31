context("render_pmap_file()")

test_that("render_pmap_file() should be able to render to a file", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 1000,
    number_of_cases = 100,
    categories = c("campaign", "sale"),
    categories_size = c(10, 4)
  )

  p <- eventlog %>% create_pmap() %>% prune_nodes(0.5) %>% prune_edges(0.5)

  for (format in c("pdf", "svg", "png", "ps")) {
    file_name <- tempfile(fileext = paste0(".", format))
    render_pmap_file(p, file_name, format = format)
    expect_true(file.exists(file_name))
  }
})

test_that("render_pmap_file() should be able to guess file format", {
  eventlog <- generate_eventlog(
    size_of_eventlog = 1000,
    number_of_cases = 100,
    categories = c("campaign", "sale"),
    categories_size = c(10, 4)
  )

  p <- eventlog %>% create_pmap() %>% prune_nodes(0.5) %>% prune_edges(0.5)

  for (format in c("pdf", "png", "dot")) {
    file_name <- tempfile(fileext = paste0(".", format))
    render_pmap_file(p, file_name)
    expect_true(file.exists(file_name))
  }
})

# test external `dot` command only if there is a `dot` command installed.
test_that("render_pmap_file() should be able to use external `dot` command", {
  t <- try(
    system2("dot", c("-V"), stdout = TRUE, stderr = TRUE),
    silent = TRUE
  )

  if (inherits(t, "try-error")) {
    skip("`dot` is not installed on this system, so this test is ignored.")
  }

  eventlog <- generate_eventlog(
    size_of_eventlog = 1000,
    number_of_cases = 100,
    categories = c("campaign", "sale"),
    categories_size = c(10, 4)
  )

  p <- eventlog %>% create_pmap() %>% prune_nodes(0.5) %>% prune_edges(0.5)

  for (format in c("pdf", "png", "svg")) {
    file_name <- tempfile(fileext = paste0(".", format))
    render_pmap_file(p, file_name, use_external_dot = TRUE)
    expect_true(file.exists(file_name))
  }
})
