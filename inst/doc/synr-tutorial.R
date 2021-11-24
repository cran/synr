## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set()
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8, 
  fig.height=7
)

## ----setup, include = FALSE---------------------------------------------------
library(synr)

## ----show_long_format---------------------------------------------------------
head(synr_exampledf_long_small)

## ----roll_up_data-------------------------------------------------------------
pg <- create_participantgroup(
  raw_df=synr_exampledf_long_small,
  n_trials_per_grapheme=2,
  id_col_name="participant_id",
  symbol_col_name="trial_symbol",
  color_col_name="response_color",
  time_col_name="response_time",
  color_space_spec="Luv"
)

## ----access_participant_data--------------------------------------------------
fetched_grapheme_data <- pg$participants[['3']]$graphemes[['A']]
fetched_grapheme_data

## ----individual_grapheme_cscore-----------------------------------------------
# fetching the consistency score of the second participant's grapheme 'A'
cscore_p2_A <- pg$participants[[2]]$graphemes[['A']]$get_consistency_score()
cscore_p2_A

## ----individual_participant_mean_cscore---------------------------------------
mean_cscore_p1 <- pg$participants[['1']]$get_mean_consistency_score()
mean_cscore_p1

## ----mean_cscores-------------------------------------------------------------
mean_cscores <- pg$get_mean_consistency_scores()
mean_cscores

## ----mean_scores_df-----------------------------------------------------------
mean_cscores <- pg$get_mean_consistency_scores()
p_ids <- pg$get_ids()
mean_scores_df <- data.frame(participant_id=p_ids, mean_consistency_score=mean_cscores)
mean_scores_df

## ----participant_numvalid-----------------------------------------------------
num_onlynonna_p2 <- pg$participants[['2']]$get_number_all_colored_graphemes()
num_onlynonna_p2

## ----pgroup_numvalid----------------------------------------------------------
num_onlynonna <- pg$get_numbers_all_colored_graphemes()
num_onlynonna

## ----ctest_summary_df---------------------------------------------------------
mean_cscores <- pg$get_mean_consistency_scores()
num_onlynonna <- pg$get_numbers_all_colored_graphemes()
p_ids <- pg$get_ids()
ctest_summary_df <- data.frame(
  participant_id=p_ids, 
  mean_consistency_score=mean_cscores,
  num_valid_graphemes=num_onlynonna
)
head(ctest_summary_df)

## ----example_validation-------------------------------------------------------
pg_large <- create_participantgroup(
  raw_df=synr_exampledf_large,
  n_trials_per_grapheme=3,
  id_col_name="participant_id",
  symbol_col_name="trial_symbol",
  color_col_name="response_color",
  color_space_spec="Luv"
)

# see separate article for explanation of why 'set.seed' is called
set.seed(1) 

# call validation method
val_df <- pg_large$check_valid_get_twcv_scores(
  min_complete_graphemes = 5,
  dbscan_eps = 20,
  dbscan_min_pts = 4,
  max_var_tight_cluster = 150,
  max_prop_single_tight_cluster = 0.6,
  safe_num_clusters = 3,
  safe_twcv = 250,
  complete_graphemes_only = TRUE,
  symbol_filter = LETTERS
)

head(val_df)

## ----combine_val_id-----------------------------------------------------------
val_id_df <- cbind(
  participant_id=pg_large$get_ids(),
  val_df
)
head(val_id_df)

## ----pgroup_filter_weekdays---------------------------------------------------
weekdays_filter <- c(
  'Monday', 'Tuesday', 'Wednesday', 'Thursday',
  'Friday', 'Saturday', 'Sunday'
)
# note that the 'large' example data (rolled up in 'pg_large')
# are used again here
cscores_weekdays <- pg_large$get_mean_consistency_scores(symbol_filter=weekdays_filter)
cscores_weekdays

## ----example_merge------------------------------------------------------------
pg <- create_participantgroup(
  raw_df=synr_exampledf_large,
  n_trials_per_grapheme=3,
  id_col_name="participant_id",
  symbol_col_name="trial_symbol",
  color_col_name="response_color",
  time_col_name="response_time",
  color_space_spec="Luv"
)

# form first data frame, with consistency scores
mean_cscores <- pg$get_mean_consistency_scores()
p_ids <- pg$get_ids()
cons_df <- data.frame(
  participant_id=p_ids, 
  mean_consistency_score=mean_cscores
)

# form second data frame, with validation-related information
val_df <- cbind(
  participant_id=pg$get_ids(), 
  pg$check_valid_get_twcv_scores()
)

# combine the two data frames, by telling R to 'link them up'
# based on the 'participant_id' column
cons_val_df <- merge(cons_df, val_df, by='participant_id')

head(cons_val_df)

## ----get_plot_demo------------------------------------------------------------
pg_large <- create_participantgroup(
  raw_df=synr_exampledf_large,
  n_trials_per_grapheme=3,
  id_col_name="participant_id",
  symbol_col_name="trial_symbol",
  color_col_name="response_color",
  color_space_spec="Luv"
)
# increase grapheme size and angle them slightly to make them easier to see,
# and only include digits and letters (excluding the weekday data in this
# example)
p6_plot <- pg_large$participants[['partA']]$get_plot(
  grapheme_size=2.2, 
  grapheme_angle=30,
  symbol_filter = c(0:9, LETTERS)
)
p6_plot

