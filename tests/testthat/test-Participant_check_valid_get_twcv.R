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
  "A participant with no graphemes is classified as invalid by check_valid_get_twcv.",
  {
  p <- Participant$new()
  res <- p$check_valid_get_twcv()
  expect_false(res$valid)
  expect_equal(res$reason_invalid, "no_color_responses")
  expect_equal(res$twcv, NA)
})


test_that(
  "A participant with just one grapheme is classified as invalid
  by check_valid_get_twcv.",
  {
  p <- Participant$new()
  g1 <- Grapheme$new(symbol='a')
  g1$set_colors(c("#0000FF", "#0000FF", "#00FF00", "#00FF00"), "Luv")
  p$add_grapheme(g1)
  res <- p$check_valid_get_twcv()
  expect_false(res$valid)
  expect_equal(res$reason_invalid, "too_few_graphemes_with_complete_responses")
  expect_equal(res$twcv, NA)
})

test_that(
  "A participant with two graphemes, of which one includes an NA
  response color, is classified as invalid by check_valid_get_twcv
  even when min_complete_graphemes is set to 1 and
  complete_graphemes_only = FALSE.",
  {
    p <- Participant$new()
    g1 <- Grapheme$new(symbol='a')
    g1$set_colors(c("#0000FF", "#0000FF", NA), "Luv")
    g2 <- Grapheme$new(symbol='b')
    g2$set_colors(c("#0000FF", "#0000FF", "#00FF00"), "Luv")
    p$add_graphemes(list(g1, g2))
    res <- p$check_valid_get_twcv(
      min_complete_graphemes = 1,
      complete_graphemes_only = FALSE
    )
    expect_false(res$valid)
    expect_equal(res$reason_invalid, "hi_prop_tight_cluster")
    expect_equal(res$twcv, 0)
  })

test_that(
  "A participant with 20 graphemes of same color is classified as invalid
  by check_valid_get_twcv.",
  {
  p <- Participant$new()
  for (l in LETTERS[1:20]) {
    g <- Grapheme$new(symbol=l)
    g$set_colors(c("#0000FF", "#0000FF", "#00FF00", "#00FF00"), "Luv")
    p$add_grapheme(g)
  }

  res <- p$check_valid_get_twcv()
  expect_false(res$valid)
  expect_equal(res$reason_invalid, "few_clusters_low_twcv")
  expect_equal(res$twcv, 0)
  }
)


test_that(
  "A participant with 20 graphemes, with 3 responses each,
  of wildly varying (randomly generated) colors is classified as valid."
  , {
    p <- Participant$new()
    for (l in LETTERS[1:20]) {
      g <- Grapheme$new(symbol=l)
      g$set_colors(c(get_random_color(), get_random_color(), get_random_color()), "Luv")
      p$add_grapheme(g)
    }
    res <- p$check_valid_get_twcv()
    expect_true(res$valid)
    expect_equal(res$reason_invalid, "")
    expect_gt(res$twcv, 500)
  }
)

test_that(
  "check_valid_get_twcv: A participant with:
  15 graphemes, with 2 responses each of wildly
  varying (randomly generated) colors, and
  8 graphemes of the same color
  is classified as invalid when 'complete_graphemes_only = TRUE' (default)."
  , {
    p <- Participant$new()
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
    res <- p$check_valid_get_twcv()
    expect_false(res$valid)
    expect_equal(res$reason_invalid, "hi_prop_tight_cluster")
    expect_lt(res$twcv, 50)
    expect_equal(res$num_clusters, 1)
  }
)

test_that(
  "check_valid_get_twcv: A participant with:
  15 graphemes, with 2 responses each of wildly
  varying (randomly generated) colors, and
  8 graphemes of the same color
  is classified as valid when 'complete_graphemes_only = FALSE'."
  , {
    p <- Participant$new()
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
    res <- p$check_valid_get_twcv(complete_graphemes_only = FALSE)
    expect_true(res$valid)
    expect_equal(res$reason_invalid, "")
    expect_gt(res$twcv, 200)
    expect_gt(res$num_clusters, 1)
  }
)
