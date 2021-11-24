context("Color setting and description")
library(synr)

test_that("Only non NA colors are counted as such", {
  a <- synr::Grapheme$new(symbol='a')
  a$set_colors(c("#101010", NA, NA, "#DDBBAA", NA), "Luv")
  expect_equal(a$get_num_non_na_colors(), 2)
  b <- synr::Grapheme$new(symbol='b')
  b$set_colors(c(NA), "Luv")
  expect_equal(b$get_num_non_na_colors(), 0)
  foo <- synr::Grapheme$new(symbol='foo')
  foo$set_colors(c("#880033"), "Luv")
  expect_equal(foo$get_num_non_na_colors(), 1)
})


test_that("has_only_non_na() returns TRUE when this is the case", {
  foo <- synr::Grapheme$new(symbol='foo')
  foo$set_colors(c("#880033"), "Luv")
  expect_true(foo$has_only_non_na_colors())
})


test_that("Color space is correctly stored", {
  a <- synr::Grapheme$new(symbol='a')
  a$set_colors(c("#101010", NA, NA, "#DDBBAA", NA), "Luv")
  expect_equal(a$color_space, "Luv")
})


test_that("Conversion to Luv and back to hex code using Grapheme.get_hex_colors() works", {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000DD", "#0000FF", "#00FF00", "#CC0000"), "Luv")
            reconverted_colors <- a$get_hex_colors()
            expect_equal(reconverted_colors[1], "#0000DD")
            expect_equal(reconverted_colors[2], "#0000FF")
            expect_equal(reconverted_colors[3], "#00FF00")
            expect_equal(reconverted_colors[4], "#CC0000")
          })


test_that("ParticipantGroup.get_numbers_all_colored_graphemes() returns correct number of graphemes
          with all colored responses for 1 participant", {
            g1 <- Grapheme$new(symbol="a")
            g2 <- Grapheme$new(symbol="b")
            g1$set_colors(c("#800020", "#F08000", "#993322"), "Luv")
            g2$set_colors(c(NA, "#F08000", "#993322"), "Luv")

            p1 <- Participant$new(id="1")
            p1$add_graphemes(list(g1, g2))

            pl <- ParticipantGroup$new()
            pl$add_participants(list(p1))

            expect_equal(pl$get_numbers_all_colored_graphemes()[1], 1)
          })


test_that("ParticipantGroup.get_numbers_all_colored_graphemes() returns correct number of graphemes
          with all colored graphemes for 2 participants", {
            g1 <- Grapheme$new(symbol="a")
            g2 <- Grapheme$new(symbol="b")
            g1$set_colors(c("#800020", "#F08000", "#993322"), "Luv")
            g2$set_colors(c(NA, "#F08000", "#993322"), "Luv")
            g3 <- Grapheme$new(symbol="a")
            g4 <- Grapheme$new(symbol="b")
            g3$set_colors(c("#800020", "#F08000", "#993322"), "Luv")
            g4$set_colors(c(NA, NA, NA), "Luv")

            p1 <- Participant$new(id="1")
            p1$add_graphemes(list(g1, g2))
            p2 <- Participant$new(id="2")
            p2$add_graphemes(list(g3, g4))

            pl <- ParticipantGroup$new()
            pl$add_participants(list(p1, p2))

            expect_equal(pl$get_numbers_all_colored_graphemes()[1], 1)
            expect_equal(pl$get_numbers_all_colored_graphemes()[2], 1)
          })


test_that("ParticipantGroup.get_numbers_all_colored_graphemes() returns correct number of graphemes
          with all colored graphemes for 2 participants, when using symbol_filter", {
            g1 <- Grapheme$new(symbol="a")
            g2 <- Grapheme$new(symbol="b")
            g1$set_colors(c("#800020", "#F08000", "#993322"), "Luv")
            g2$set_colors(c(NA, "#F08000", "#993322"), "Luv")
            g3 <- Grapheme$new(symbol="a")
            g4 <- Grapheme$new(symbol="b")
            g3$set_colors(c("#800020", "#F08000", "#993322"), "Luv")
            g4$set_colors(c(NA, NA, NA), "Luv")

            p1 <- Participant$new(id="1")
            p1$add_graphemes(list(g1, g2))
            p2 <- Participant$new(id="2")
            p2$add_graphemes(list(g3, g4))

            pl <- ParticipantGroup$new()
            pl$add_participants(list(p1, p2))

            expect_equal(pl$get_numbers_all_colored_graphemes(symbol_filter=c("a"))[1], 1)
            expect_equal(pl$get_numbers_all_colored_graphemes(symbol_filter=c("b"))[2], 0)
          })
