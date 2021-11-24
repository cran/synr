context("Participant initiating and adding graphemes, and fetching basic information about them.")
library(synr)


test_that("has_graphemes() returns False if there are no graphemes", {
  p <- Participant$new()
  expect_false(p$has_graphemes())
})

test_that("add_grapheme() raises warning if passed grapheme has no associated symbol", {
  p <- Participant$new()
  g <- Grapheme$new()
  expect_error(p$add_grapheme(g))
})

test_that("add_graphemes() adds all passed graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g2 <- Grapheme$new(symbol='b')
  g3 <- Grapheme$new(symbol='monday')
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_equal(p$graphemes[[2]], g_list[[2]])
})

test_that("has_graphemes() returns True if there are graphemes", {
  p <- Participant$new()
  g <- Grapheme$new(symbol='a')
  p$add_grapheme(g)
  expect_true(p$has_graphemes())
})

test_that("get_symbols() returns correct set of symbols", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g2 <- Grapheme$new(symbol='b')
  g3 <- Grapheme$new(symbol='monday')
  p$add_grapheme(g1)
  p$add_grapheme(g2)
  p$add_grapheme(g3)
  expect_equal(p$get_symbols(), c('a', 'b', 'monday'))
})

test_that("get_number_all_colored_graphemes returns 3 for participant with 3 all-valid response colors graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_equal(p$get_number_all_colored_graphemes(), 3)
})

test_that("get_number_all_colored_graphemes returns 0 for participant with no graphemes", {
  p <- Participant$new()
  expect_equal(p$get_number_all_colored_graphemes(), 0)
})

test_that("get_number_all_colored_graphemes returns 0 for participant with only graphemes without responses", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g2 <- Grapheme$new(symbol='b')
  g3 <- Grapheme$new(symbol='monday')
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_equal(p$get_number_all_colored_graphemes(), 0)
})

test_that("get_number_all_colored_graphemes returns 0 for participant with only graphemes with invalid responses", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c(NA, NA, NA, NA), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c(NA, NA, NA, NA), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c(NA, NA, NA, NA), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_equal(p$get_number_all_colored_graphemes(), 0)
})

test_that(paste0(
  "get_nonna_color_resp_mat produces expected matrix ",
  "for participant with 12 valid responses"
), {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#0000FF", "#0000FF", "#00FF00", "#00FF00"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#AA0000", "#000000", "#AA0000", "#BB0000"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#00FF00", "#00EE00", "#00BB00", "#110000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  col_mat <- p$get_nonna_color_resp_mat()
  expect_equal(nrow(col_mat), 12)
})
