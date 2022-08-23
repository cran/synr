context("ParticipantGroup .get_mean_colors method")
library(synr)

test_that("get_mean_colors() returns correct mean color axis values, sRGB, na.rm=TRUE", {
  p1 <- Participant$new(id="1")
  r_vals <- c(0.4, 0.5, 0.6)
  g_vals <- c(0, 0, 0)
  b_vals <- c(1, 1, 1)
  p1_exp_vals <- c(mean(r_vals), mean(g_vals), mean(b_vals))
  color_vec <- c()
  for (i in 1:length(r_vals)) {
    color_vec <- c(color_vec, rgb(r_vals[i], g_vals[i], b_vals[i]))
  }
  g <- Grapheme$new(symbol="A")
  g$set_colors(color_vec, "sRGB")
  p1$add_grapheme(g)

  p2 <- Participant$new(id="2")
  r_vals <- c(NA, 0.5, 0.6)
  g_vals <- c(NA, 0, 0)
  b_vals <- c(NA, 1, 1)
  p2_exp_vals <- c(
    mean(r_vals, na.rm=TRUE),
    mean(g_vals, , na.rm=TRUE),
    mean(b_vals, , na.rm=TRUE)
  )
  color_vec <- c()
  for (i in 1:length(r_vals)) {
    if (any(sapply(c(r_vals[i], g_vals[i], b_vals[i]), is.na))) {
      color_vec <- c(color_vec, NA)
    } else {
      color_vec <- c(color_vec, rgb(r_vals[i], g_vals[i], b_vals[i]))
    }
  }
  g <- Grapheme$new(symbol="A")
  g$set_colors(color_vec, "sRGB")
  p2$add_grapheme(g)

  p3 <- Participant$new(id="3")
  r_vals <- c(NA, NA, NA)
  g_vals <- c(NA, NA, NA)
  b_vals <- c(NA, NA, NA)
  p3_exp_vals <- c(
    mean(r_vals, na.rm=TRUE),
    mean(g_vals, , na.rm=TRUE),
    mean(b_vals, , na.rm=TRUE)
  )
  color_vec <- c()
  for (i in 1:length(r_vals)) {
    if (any(sapply(c(r_vals[i], g_vals[i], b_vals[i]), is.na))) {
      color_vec <- c(color_vec, NA)
    } else {
      color_vec <- c(color_vec, rgb(r_vals[i], g_vals[i], b_vals[i]))
    }
  }
  g <- Grapheme$new(symbol="A")
  g$set_colors(color_vec, "sRGB")
  p3$add_grapheme(g)

  pg <- ParticipantGroup$new()
  pg$add_participants(list(p1, p2, p3))
  mean_color_df <- pg$get_mean_colors(na.rm=TRUE)
  expect_equal(as.numeric(as.vector(mean_color_df[1, ])), p1_exp_vals, tolerance=0.01)
  expect_equal(as.numeric(as.vector(mean_color_df[2, ])), p2_exp_vals, tolerance=0.01)
  expect_true(all(sapply(mean_color_df[3, ], is.na)))
})

test_that("get_mean_colors() returns correct mean color axis values, sRGB, na.rm=FALSE", {
  p1 <- Participant$new(id="1")
  r_vals <- c(0.4, 0.5, 0.6)
  g_vals <- c(0, 0, 0)
  b_vals <- c(1, 1, 1)
  p1_exp_vals <- c(mean(r_vals), mean(g_vals), mean(b_vals))
  color_vec <- c()
  for (i in 1:length(r_vals)) {
    color_vec <- c(color_vec, rgb(r_vals[i], g_vals[i], b_vals[i]))
  }
  g <- Grapheme$new(symbol="A")
  g$set_colors(color_vec, "sRGB")
  p1$add_grapheme(g)

  p2 <- Participant$new(id="2")
  r_vals <- c(NA, 0.5, 0.6)
  g_vals <- c(NA, 0, 0)
  b_vals <- c(NA, 1, 1)
  p2_exp_vals <- c(
    mean(r_vals, na.rm=TRUE),
    mean(g_vals, , na.rm=TRUE),
    mean(b_vals, , na.rm=TRUE)
  )
  color_vec <- c()
  for (i in 1:length(r_vals)) {
    if (any(sapply(c(r_vals[i], g_vals[i], b_vals[i]), is.na))) {
      color_vec <- c(color_vec, NA)
    } else {
      color_vec <- c(color_vec, rgb(r_vals[i], g_vals[i], b_vals[i]))
    }
  }
  g <- Grapheme$new(symbol="A")
  g$set_colors(color_vec, "sRGB")
  p2$add_grapheme(g)

  p3 <- Participant$new(id="3")
  r_vals <- c(NA, NA, NA)
  g_vals <- c(NA, NA, NA)
  b_vals <- c(NA, NA, NA)
  p3_exp_vals <- c(
    mean(r_vals, na.rm=TRUE),
    mean(g_vals, , na.rm=TRUE),
    mean(b_vals, , na.rm=TRUE)
  )
  color_vec <- c()
  for (i in 1:length(r_vals)) {
    if (any(sapply(c(r_vals[i], g_vals[i], b_vals[i]), is.na))) {
      color_vec <- c(color_vec, NA)
    } else {
      color_vec <- c(color_vec, rgb(r_vals[i], g_vals[i], b_vals[i]))
    }
  }
  g <- Grapheme$new(symbol="A")
  g$set_colors(color_vec, "sRGB")
  p3$add_grapheme(g)

  pg <- ParticipantGroup$new()
  pg$add_participants(list(p1, p2, p3))
  mean_color_df <- pg$get_mean_colors(na.rm=FALSE)
  expect_equal(as.numeric(as.vector(mean_color_df[1, ])), p1_exp_vals, tolerance=0.01)
  expect_true(all(sapply(mean_color_df[2, ], is.na)))
  expect_true(all(sapply(mean_color_df[3, ], is.na)))
})

