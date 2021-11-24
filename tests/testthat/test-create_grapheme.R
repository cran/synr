context("Grapheme creation")
library(synr)

test_that("create_grapheme creates a valid grapheme", {
  g <- create_grapheme(symbol="Monday", response_times=c(2.3, 6.7, 0.4),
                  response_colors=c("84AE99", "9E3300", "000000"), "Luv")
  expect_equal(g$get_abbreviated_symbol(), "Mon")
  expect_equal(g$get_num_non_na_colors(), 3)
  expect_gt(g$get_consistency_score(), 100)
})
