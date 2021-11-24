context("Plotting related functions")
library(synr)

test_that("Grapheme.get_plot_data_list() returns correctly formatted data when all response colors are valid", {
  g <- synr::Grapheme$new(symbol='Monday')
  g$set_colors(c("#00DE00", "#00FF00", "#390000"), "Luv")
  data_ls <- g$get_plot_data_list()
  expect_equal(data_ls[[1]], 'Mon')
  expect_gt(data_ls[[2]], 200)
  expect_equal(data_ls[[3]], "#00DE00")
  expect_equal(data_ls[[4]], "#00FF00")
  expect_equal(data_ls[[5]], "#390000")
})

test_that("Grapheme.get_plot_data_list() returns correctly formatted data when one response color is invalid", {
  g <- synr::Grapheme$new(symbol='Monday')
  g$set_colors(c("#00DE00", NA, "#390000"), "Luv")
  data_ls <- g$get_plot_data_list()
  expect_equal(data_ls[[1]], 'Mon')
  expect_true(is.na(data_ls[[2]]) )
  expect_equal(data_ls[[3]], "#00DE00")
  expect_true(is.na(data_ls[[4]]) )
  expect_equal(data_ls[[5]], "#390000")
})

test_that("Participant.get_plot_data() returns correctly formatted data for participant with a single grapheme with valid data", {
  p <- Participant$new()
  g <- synr::Grapheme$new(symbol='Monday')
  g$set_colors(c("#00DE00", "#00FF00", "#390000"), "Luv")
  p$add_grapheme(g)
  plot_df <- p$get_plot_data()
  expect_equal(as.character(plot_df[['symbol']][1]), 'Mon')
  expect_lt(abs(plot_df[['consistency_score']][1] - 339.0621), 0.01)
  expect_equal(plot_df[['color_resp_1']][1], "#00DE00")
  expect_equal(plot_df[['color_resp_2']][1], "#00FF00")
  expect_equal(plot_df[['color_resp_3']][1], "#390000")
})

test_that("Participant.get_plot_data() returns correctly formatted data for participant with two graphemes with valid data", {
  p <- Participant$new(id="kalleanka")
  g1 <- synr::Grapheme$new(symbol='Monday')
  g1$set_colors(c("#00DE00", "#00FF00", "#390000"), "Luv")
  p$add_grapheme(g1)
  g2 <- synr::Grapheme$new(symbol='9')
  g2$set_colors(c("#00DE00", "#00FF00", "#FF2200"), "Luv")
  p$add_grapheme(g2)
  plot_df <- p$get_plot_data()
  # note that data are to be sorted in alphabetically __descending__ order,
  # so that they 'increase' from top-to-bottom when used in
  # plotting function
  expect_equal(as.character(plot_df[['symbol']][1]), 'Mon')
  expect_lt(abs(plot_df[['consistency_score']][1] - 339.0621), 0.01)
  expect_equal(plot_df[['color_resp_1']][1], "#00DE00")
  expect_equal(plot_df[['color_resp_2']][1], "#00FF00")
  expect_equal(plot_df[['color_resp_3']][1], "#390000")

  expect_equal(as.character(plot_df[['symbol']][2]), '9')
  expect_lt(abs(plot_df[['consistency_score']][2] - 531.4201), 0.01)
  expect_equal(plot_df[['color_resp_1']][2], "#00DE00")
  expect_equal(plot_df[['color_resp_2']][2], "#00FF00")
  expect_equal(plot_df[['color_resp_3']][2], "#FF2200")

})

test_that("Participant.get_plot_data() returns correctly formatted data for participant with a single grapheme with invalid data", {
  p <- Participant$new()
  g <- synr::Grapheme$new(symbol='Monday')
  g$set_colors(c(NA, "#00FF00", "#390000"), "Luv")
  p$add_grapheme(g)
  plot_df <- p$get_plot_data()
  expect_equal(as.character(plot_df[['symbol']][1]), 'Mon')
  expect_true(is.na(plot_df[['consistency_score']][1]) )
  expect_true(is.na(plot_df[['color_resp_1']][1]) )
  expect_equal(plot_df[['color_resp_2']][1], "#00FF00")
  expect_equal(plot_df[['color_resp_3']][1], "#390000")
})

test_that("Participant.get_plot_data() produces error when called for participant with no graphemes", {
  p <- Participant$new()
  expect_error(p$get_plot_data())
})
