context("Grapheme misc. functionality")
library(synr)

test_that("Symbol is set and represented correctly ", {
  g1 <- Grapheme$new(symbol="a")
  expect_equal(g1$symbol, "a")
  g2 <- Grapheme$new()
  g2$set_symbol('b')
  expect_equal(g2$symbol, "b")
})

test_that("Abbreviated symbol representation is correct", {
  g1 <- Grapheme$new(symbol="a")
  expect_equal(g1$get_abbreviated_symbol(), "a")
  g2 <- Grapheme$new()
  g2$set_symbol('Monday')
  expect_equal(g2$get_abbreviated_symbol(), "Mon")
})

test_that("get_mean_color works correctly", {
  a <- synr::Grapheme$new(symbol='a')
  b <- synr::Grapheme$new(symbol='b')
  d <- synr::Grapheme$new(symbol='d')
  a$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  b$set_colors(c("#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF"), "Luv")
  d$set_colors(c("#FAFAFA", "#000000", "#FFFFFF", "#FF0000"), "Luv")
  expect_equal(
    a$get_mean_color(),
    c(0, 0, 0)
  )
  expect_equal(
    d$get_mean_color(),
    c(62.94, 43.84, 9.45),
    tolerance=0.01
  )
})
