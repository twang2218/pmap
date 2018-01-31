context("adjust_edge_label()")

test_that("adjust_edge_label() handle graph change the edge label", {
  p <- create_pmap(
    data.frame(
      timestamp = c(
        "2017-07-01",
        "2017-07-02",
        "2017-07-03",
        "2017-07-04",
        "2017-07-05",
        "2017-07-06",
        "2017-07-20"
      ),
      case_id = c("c1", "c1", "c1", "c2", "c2", "c3", "c3"),
      activity = c("a", "b", "a", "b", "b", "a", "b"),
      category = c("campaign", "sale", "campaign", "sale", "sale", "campaign", "sale"),
      stringsAsFactors = FALSE
    )
  )

  edge_df <- DiagrammeR::get_edge_df(p)
  expect_equal(edge_df$label, c("   2   ", "   1   ", "   1   "))

  p1 <- adjust_edge_label(p, label = "max_duration")
  edge_df <- DiagrammeR::get_edge_df(p1)
  expect_equal(edge_df$label, c("   2 weeks   ", "   1 days   ", "   1 days   "))

  p1 <- adjust_edge_label(p, label = "min_duration")
  edge_df <- DiagrammeR::get_edge_df(p1)
  expect_equal(edge_df$label, c("   1 days   ", "   1 days   ", "   1 days   "))

  p1 <- adjust_edge_label(p, label = "mean_duration")
  edge_df <- DiagrammeR::get_edge_df(p1)
  expect_equal(edge_df$label, c("   1.07 weeks   ", "   1 days   ", "   1 days   "))

  p1 <- adjust_edge_label(p, label = "amount")
  edge_df <- DiagrammeR::get_edge_df(p1)
  expect_equal(edge_df$label, c("   2   ", "   1   ", "   1   "))
})

