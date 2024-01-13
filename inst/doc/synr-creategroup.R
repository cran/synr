## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(synr)

## -----------------------------------------------------------------------------
synr_exampledf_long_small

## -----------------------------------------------------------------------------
pg <- create_participantgroup(
  raw_df=synr_exampledf_long_small,
  n_trials_per_grapheme=2,
  id_col_name="participant_id",
  symbol_col_name="trial_symbol",
  color_col_name="response_color",
  time_col_name="response_time",
  color_space_spec="Luv"
)

## -----------------------------------------------------------------------------
synr_exampledf_wide_small

## -----------------------------------------------------------------------------
pg <- create_participantgroup_widedata(
  raw_df=synr_exampledf_wide_small,
  n_trials_per_grapheme=2,
  participant_col_name="participant_id",
  symbol_col_regex="symbol",
  color_col_regex="colou*r",
  time_col_regex="response_time",
  color_space_spec="Luv"
)

