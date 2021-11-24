# helper function for testing
get_random_color <- function() {
  r_val <- runif(1, 0, 1)
  g_val <- runif(1, 0, 1)
  b_val <- runif(1, 0, 1)
  alpha_val <- runif(1, 0, 1)
  hex_val <- rgb(r_val, g_val, b_val, alpha_val)
  return(hex_val)
}

test_that(
  "check_valid_get_twcv_scores classifies participant with no graphemes
  as invalid.", {
  p <- Participant$new(id="1")
  pg <- ParticipantGroup$new()
  pg$add_participant(p)
  val_res <- pg$check_valid_get_twcv_scores()
  expect_false(val_res$valid[1])
})

test_that(
  "check_valid_get_twcv_scores classifies two participants with no graphemes
  as both being invalid.", {
    p1 <- Participant$new(id="1")
    p2 <- Participant$new(id="2")
    pg <- ParticipantGroup$new()
    pg$add_participants(list(p1, p2))
    val_res <- pg$check_valid_get_twcv_scores()
    expect_false(val_res$valid[1])
    expect_false(val_res$valid[2])
    expect_equal(val_res$reason_invalid[2], "no_color_responses")
    expect_equal(val_res$twcv[2], as.double(c(NA)))
})

test_that(
  "check_valid_get_twcv_scores classifies participant with just one grapheme
  as invalid.", {
    p <- Participant$new(id="1")
    g1 <- Grapheme$new(symbol='a')
    g1$set_colors(c("#0000FF", "#0000FF", "#00FF00", "#00FF00"), "Luv")
    p$add_grapheme(g1)
    pg <- ParticipantGroup$new()
    pg$add_participant(p)

    val_res <- pg$check_valid_get_twcv_scores()
    expect_false(val_res$valid[1])
    expect_equal(val_res$reason_invalid[1], "too_few_graphemes_with_complete_responses")
    expect_equal(val_res$twcv[1], as.double(c(NA)))
})

test_that(
  "check_valid_get_twcv_scores classifies participant with 20 graphemes
  of same color as invalid.", {
    p <- Participant$new(id="1")
    for (l in LETTERS[1:20]) {
      g <- Grapheme$new(symbol=l)
      g$set_colors(c("#0000FF", "#0000FF", "#00FF00", "#00FF00"), "Luv")
      p$add_grapheme(g)
    }
    pg <- ParticipantGroup$new()
    pg$add_participant(p)

    val_res <- pg$check_valid_get_twcv_scores()
    expect_false(val_res$valid[1])
    expect_equal(val_res$reason_invalid[1], "few_clusters_low_twcv")
    expect_equal(val_res$twcv[1], 0)
})

test_that(
  "check_valid_get_twcv_scores classifies participant with 20 graphemes,
  with 3 responses each, of wildly varying (randomly generated)
  colors as invalid.", {
    p <- Participant$new(id="1")
    for (l in LETTERS[1:20]) {
      g <- Grapheme$new(symbol=l)
      g$set_colors(c(get_random_color(), get_random_color(), get_random_color()), "Luv")
      p$add_grapheme(g)
    }
    pg <- ParticipantGroup$new()
    pg$add_participant(p)

    val_res <- pg$check_valid_get_twcv_scores()
    expect_true(val_res$valid[1])
    expect_equal(val_res$reason_invalid[1], "")
    expect_gt(val_res$twcv[1], 500)
})


test_that(
  "check_valid_get_twcv_scores: A participant with:
  15 graphemes, with 2 responses each of wildly
  varying (randomly generated) colors, and
  8 graphemes of the same color
  is classified as valid when 'complete_graphemes_only = FALSE',
  and invalid when 'complete_graphemes_only = TRUE'."
  , {
    p <- Participant$new(id="1")
    for (l in LETTERS[1:15]) {
      g <- Grapheme$new(symbol=l)
      g$set_colors(c(get_random_color(), get_random_color(), NA), "Luv")
      p$add_grapheme(g)
    }
    for (l in LETTERS[16:23]) {
      g <- Grapheme$new(symbol=l)
      g$set_colors(c("#FFFFFF", "#FFFFFF", "#FFFFFF"), "Luv")
      p$add_grapheme(g)
    }
    pg <- ParticipantGroup$new()
    pg$add_participant(p)

    res_df_not_only_complete <- pg$check_valid_get_twcv_scores(complete_graphemes_only = FALSE)
    expect_true(res_df_not_only_complete$valid[1])

    res_df_only_complete <- pg$check_valid_get_twcv_scores(complete_graphemes_only = TRUE)
    expect_false(res_df_only_complete$valid[1])

    expect_equal(res_df_only_complete$num_clusters[1], 1)
  }
)
