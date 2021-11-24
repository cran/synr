context("Converting raw consistency test data to a participant group with all the data bundled up")
library(synr)

test_that("a single row of data (split into symbol/color/time vectors)
          is correctly converted using create_participant()", {
  participant_id <- "1"
  target_symbols_vec <- c("A", "D", "7")
  symbol_vec <- c("A", "D", "7",
                  "D", "A", "7",
                  "7", "A", "D")
  times_vec <- c(1.1, 0.4, 5,
                 0.3, 2.4, 7.3,
                 1, 10.2, 8.4)
  color_vec <- c("98FF22", "138831", "791322",
                 "8952FE", "DC8481", "7D89B0",
                 "001100", "887755", "FF0033")
  p <- create_participant(participant_id=participant_id,
                          grapheme_symbols=target_symbols_vec,
                          n_trials_per_grapheme=3,
                          trial_symbols=symbol_vec,
                          response_times=times_vec,
                          response_colors=color_vec)
  expect_equal(p$get_symbols()[1], "A")
  expect_gt(p$get_mean_consistency_score(), 100)
})

test_that("a single row of data (split into symbol/color/time vectors)
          is correctly converted using create_participant(), even
          if there are some NA values in the response color data", {
            participant_id <- "1"
            target_symbols_vec <- c("A", "D", "7")
            symbol_vec <- c("A", "D", "7",
                            "D", "A", "7",
                            "7", "A", "D")
            times_vec <- c(1.1, 0.4, 5,
                           0.3, 2.4, 7.3,
                           1, 10.2, 8.4)
            color_vec <- c("98FF22", NA, NA,
                           "8952FE", "DC8481", "7D89B0",
                           "001100", "887755", "FF0033")
            p <- create_participant(participant_id=participant_id,
                                    grapheme_symbols=target_symbols_vec,
                                    n_trials_per_grapheme=3,
                                    trial_symbols=symbol_vec,
                                    response_times=times_vec,
                                    response_colors=color_vec)
            expect_equal(p$get_symbols()[1], "A")
          })

test_that("synr_exampledf_wide_small is correctly converted into a participantgroup when using
          create_participantgroup_widedata() and the resulting group produces correct mean consistency
          scores", {
            pg <- create_participantgroup_widedata(raw_df=synr_exampledf_wide_small,
                                          n_trials_per_grapheme=2,
                                          participant_col_name="participant_id",
                                          symbol_col_regex="symbol",
                                          color_col_regex="colou*r",
                                          time_col_regex="response_time",
                                          color_space_spec="Luv"
                                          )
            cons_means <- pg$get_mean_consistency_scores()
            expect_equal(length(cons_means), 3)
            expect_gt(cons_means[1], 100)
          })

test_that("synr_exampledf_long_small is converted into a participantgroup by
          create_participantgroup() and the resulting group
          can produce mean consistency scores", {
  pg <- create_participantgroup(
    raw_df=synr_exampledf_long_small,
    n_trials_per_grapheme=2,
    id_col_name="participant_id",
    symbol_col_name="trial_symbol",
    color_col_name="response_color",
    time_col_name="response_time",
    color_space_spec="Luv"
  )
  cons_means <- pg$get_mean_consistency_scores()
  expect_equal(length(cons_means), 3)
  expect_gt(cons_means[1], 100)
})
