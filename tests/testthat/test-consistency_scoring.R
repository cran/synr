context("Consistency scoring")
library(synr)

test_that("Grapheme All black color responses leads to a consistency score near 0",
          {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            expect_lt(a$get_consistency_score(),
                         1)
          })


test_that("Grapheme All perfect blue color responses leads to a consistency score near 0",
          {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000FF", "#0000FF", "#0000FF", "#0000FF"), "Luv")
            expect_lt(a$get_consistency_score(),
                         1)
          })


test_that("Grapheme 3 very different color responses leads to a consistency score above 135",
          {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c("#0000FF", "#00FF00", "#FF0000"), "Luv")
            expect_gt(a$get_consistency_score(),
                         135)
          })


test_that("Grapheme All NA color responses leads to a NA consistency score",
          {
            a <- synr::Grapheme$new(symbol='a')
            a$set_colors(c(NA, NA, NA), "Luv")
            is_consistency_na <- is.na(a$get_consistency_score())
            expect_true(is_consistency_na)
          })


test_that("Grapheme Fetching consistency score before inserting responses
          produces error",
          {
            a <- synr::Grapheme$new(symbol='a')
            expect_error(a$get_consistency_score(),
                         paste0("This grapheme, with symbol ", a$symbol,
                                ' has no registered response colors. Please register responses before calling .get_consistency_score().')
                         )
          })


test_that("Participant get_consistency_scores() returns correct list for valid response graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#FFFFFF", "#000000", "#FFFFFF", "#000000"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  cons_list <- p$get_consistency_scores()
  expect_lt(cons_list[['b']], 0.01)
  expect_lt(cons_list[[2]], 0.01)
  expect_gt(cons_list[['a']], 100)
})


test_that("Participant get_consistency_scores() returns correct list for nonvalid response graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c(NA, NA, NA, NA), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c(NA, NA, NA, NA), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c(NA, NA, NA, NA), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  cons_list <- p$get_consistency_scores()
  expect_true(is.na(cons_list[['b']]))
  expect_true(is.na(cons_list[[2]]))
  expect_true(is.na(cons_list[['a']]))
})

test_that("Participant get_consistency_scores() returns correct list for valid response graphemes,
          when using filter", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#FFFFFF", "#000000", "#FFFFFF", "#000000"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  cons_list <- p$get_consistency_scores(symbol_filter=c("a", "monday"))
  expect_equal(length(cons_list), 2)
  expect_lt(cons_list[['monday']], 0.01)
  expect_gt(cons_list[['a']], 100)
})

test_that("Participant get_mean_consistency_score() returns value close to 0 for all black response graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_lt(p$get_mean_consistency_score(), 0.01)
})

test_that("Participant get_mean_consistency_score() returns higher than 200 for very mixed response graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#FFFFFF", "#000000", "#FFFFFF", "#000000"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#000000", "#FFFFFF", "#000000", "#FFFFFF"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#0000FF", "#000000", "#FF0000", "#000000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_gt(p$get_mean_consistency_score(), 200)
})

test_that("Participant get_mean_consistency_score() returns correct
          values based on what's used for the symbol filter", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#FFFFFF", "#FF0000", "#FFAAFF", "#000000"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#000000", "#FFFFFF", "#000000", "#FFFFFF"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_gt(p$get_mean_consistency_score(), 200)
  expect_lt(p$get_mean_consistency_score(symbol_filter=c("monday")), 1)
  expect_gt(p$get_mean_consistency_score(symbol_filter=c("a", "b", "monday")), 200)
  expect_gt(p$get_mean_consistency_score(symbol_filter=c("a", "b")), 500)
})

test_that("Participant get_mean_consistency_score() returns NA value for no valid responses graphemes", {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c(NA, NA, NA, NA), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c(NA, NA, NA, NA), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c(NA, NA, NA, NA), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)
  expect_true(is.na(p$get_mean_consistency_score()))
})


test_that("ParticipantGroup get_mean_consistency_scores() with 1 participant
          returns value close to 0 for all black response graphemes", {
  p <- Participant$new(id='1')
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g2 <- Grapheme$new(symbol='b')
  g2$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g3 <- Grapheme$new(symbol='monday')
  g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
  g_list <- list(g1, g2, g3)
  p$add_graphemes(g_list)

  pg <- ParticipantGroup$new()
  pg$add_participants(list(p))
  expect_lt(pg$get_mean_consistency_scores()[1], 0.01)
})


test_that("ParticipantGroup get_mean_consistency_scores() with 2 participants
          returns value higher than 200 for very mixed response graphemes", {
            p1 <- Participant$new(id='1')
            g1 <- Grapheme$new(symbol='a')
            g1$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            g2 <- Grapheme$new(symbol='b')
            g2$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            g3 <- Grapheme$new(symbol='monday')
            g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            g_list1 <- list(g1, g2, g3)
            p1$add_graphemes(g_list1)

            p2 <- Participant$new(id='2')
            g4 <- Grapheme$new(symbol='a')
            g4$set_colors(c("#FFFFFF", "#000000", "#FFFFFF", "#000000"), "Luv")
            g5 <- Grapheme$new(symbol='b')
            g5$set_colors(c("#000000", "#FFFFFF", "#000000", "#FFFFFF"), "Luv")
            g6 <- Grapheme$new(symbol='monday')
            g6$set_colors(c("#0000FF", "#000000", "#FF0000", "#000000"), "Luv")
            g_list2 <- list(g4, g5, g6)
            p2$add_graphemes(g_list2)

            pg <- ParticipantGroup$new()
            pg$add_participants(list(p1, p2))
            expect_lt(pg$get_mean_consistency_scores()[1], 0.01)
            expect_gt(pg$get_mean_consistency_scores()[2], 200)
          })


test_that("ParticipantGroup.get_mean_consistency_scores() functions correctly with
          2 participants, where two have missing data", {
            p1 <- Participant$new(id='1')
            g1 <- Grapheme$new(symbol='a')
            g1$set_colors(c(NA, "#000000", "#000000", "#000000"), "Luv")
            g2 <- Grapheme$new(symbol='b')
            g2$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            g3 <- Grapheme$new(symbol='monday')
            g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            g_list1 <- list(g1, g2, g3)
            p1$add_graphemes(g_list1)

            p2 <- Participant$new(id='2')
            g4 <- Grapheme$new(symbol='a')
            g4$set_colors(c("#FFFFFF", "#000000", "#FFFFFF", "#000000"), "Luv")
            g5 <- Grapheme$new(symbol='b')
            g5$set_colors(c("#000000", "#FFFFFF", "#000000", "#FFFFFF"), "Luv")
            g6 <- Grapheme$new(symbol='monday')
            g6$set_colors(c("#0000FF", "#000000", "#FF0000", "#000000"), "Luv")
            g_list2 <- list(g4, g5, g6)
            p2$add_graphemes(g_list2)

            p3 <- Participant$new(id='3')
            g7 <- Grapheme$new(symbol='a')
            g7$set_colors(c(NA, "#000000", "#000000", "#000000"), "Luv")
            g8 <- Grapheme$new(symbol='b')
            g8$set_colors(c("#000000", NA, "#000000", "#000000"), "Luv")
            g9 <- Grapheme$new(symbol='monday')
            g9$set_colors(c(NA, NA, NA, NA), "Luv")
            g_list3 <- list(g7, g8, g9)
            p3$add_graphemes(g_list3)

            pg <- ParticipantGroup$new()
            pg$add_participants(list(p1, p2, p3))
            narm_scores <- pg$get_mean_consistency_scores(na.rm=TRUE)
            non_narm_scores <- pg$get_mean_consistency_scores(na.rm=FALSE)
            expect_lt(narm_scores[1], 0.01)
            expect_gt(narm_scores[2], 200)
            expect_true(is.na(narm_scores[3]))
            expect_true(is.na(non_narm_scores[1]))
          })


test_that("ParticipantGroup get_mean_consistency_scores() with 2 participants
          functions correctly when symbol_filter is being used", {
            p1 <- Participant$new(id='1')
            g1 <- Grapheme$new(symbol='a')
            g1$set_colors(c("#0000FF", "#000000", "#000000", "#000000"), "Luv")
            g2 <- Grapheme$new(symbol='b')
            g2$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            g3 <- Grapheme$new(symbol='monday')
            g3$set_colors(c("#000000", "#000000", "#000000", "#000000"), "Luv")
            g_list1 <- list(g1, g2, g3)
            p1$add_graphemes(g_list1)

            p2 <- Participant$new(id='2')
            g4 <- Grapheme$new(symbol='a')
            g4$set_colors(c("#FFFFFF", "#000000", "#FFFFFF", "#000000"), "Luv")
            g5 <- Grapheme$new(symbol='b')
            g5$set_colors(c("#000000", "#FFFFFF", "#000000", "#FFFFFF"), "Luv")
            g6 <- Grapheme$new(symbol='monday')
            g6$set_colors(c("#0000FF", "#000000", "#FF0000", "#000000"), "Luv")
            g_list2 <- list(g4, g5, g6)
            p2$add_graphemes(g_list2)

            pg <- ParticipantGroup$new()
            pg$add_participants(list(p1, p2))
            mean_cons_a <- pg$get_mean_consistency_scores(symbol_filter=c("a"))
            mean_cons_monday <- pg$get_mean_consistency_scores(symbol_filter=c("monday"))


            expect_gt(mean_cons_a[1], 30)
            expect_lt(mean_cons_a[1], 500)
            expect_lt(mean_cons_monday[1], 0.01)
            expect_gt(mean_cons_monday[2], 30)


          })
