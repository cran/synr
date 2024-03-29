## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6, 
  fig.height=5
)

## ----setup--------------------------------------------------------------------
library(synr)

## ----roll_up_data-------------------------------------------------------------
pg <- create_participantgroup(
  raw_df=synr_exampledf_long_small,
  n_trials_per_grapheme=2,
  id_col_name="participant_id",
  symbol_col_name="trial_symbol",
  color_col_name="response_color",
  time_col_name="response_time", # optional, not necessary for core functionality
  color_space_spec="Luv"
)

## ----calc_cons_scores---------------------------------------------------------
cons_scores_letters <- pg$get_mean_consistency_scores(symbol_filter=LETTERS)
print(cons_scores_letters)

## ----get_single_plot----------------------------------------------------------
pg$participants[['1']]$get_plot(symbol_filter=c('A', '7'))

## ----summary_df---------------------------------------------------------------
# get mean consistency scores for all participants, filtering first by letters, then digits
mean_cscores_letters <- pg$get_mean_consistency_scores(symbol_filter=LETTERS)
mean_cscores_digits <- pg$get_mean_consistency_scores(symbol_filter=0:9)

# get number of graphemes where all response colors were non-missing, 
# filtering first by letters, then digits
# (in the example data frame, all participants have all-valid responses)
num_valid_letters <- pg$get_numbers_all_colored_graphemes(symbol_filter=LETTERS)
num_valid_digits <- pg$get_numbers_all_colored_graphemes(symbol_filter=0:9)

p_ids <- pg$get_ids()

mean_scores_df <- data.frame(
  participant_id=p_ids, 
  cscore_letters=mean_cscores_letters,
  cscore_digits=mean_cscores_digits,
  num_valid_letters=num_valid_letters,
  num_valid_digits=num_valid_digits
)
print(mean_scores_df)

