## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6, 
  fig.height=5
)

## ----setup--------------------------------------------------------------------
library(synr)

## ----extra_libraries----------------------------------------------------------
library(synr)
library(tidyr)
library(ggplot2)

## ----download_data------------------------------------------------------------
# download the 'coloured vowels' data
githuburl <- 'https://raw.githubusercontent.com/mdingemanse/colouredvowels/master/BRM_colouredvowels_voweldata.csv'
dingemanse_voweldata <- read.csv(githuburl, sep=' ')

## ----reformat_raw_data--------------------------------------------------------
# 'pivot' the data into a long (one row per observation/trial) format,
# using tidyr's pivot_longer function (and the 'pipe' %>% operator)
cvow_long <- dingemanse_voweldata %>% 
  pivot_longer(
    cols=c('color1', 'color2', 'color3',
      'timing1', 'timing2', 'timing3'),
    names_to=c(".value", "trial"),
    names_pattern="(\\w*)(\\d)",
    values_to=c('color', 'timing')
  )
print(head(cvow_long))

## ----roll_up_data-------------------------------------------------------------
pg <- create_participantgroup(
  raw_df=cvow_long,
  n_trials_per_grapheme=3,
  id_col_name="anonid",
  symbol_col_name="item",
  color_col_name="color",
  time_col_name="timing",
  color_space_spec="Luv"
)

## ----example_plot-------------------------------------------------------------
example_plot <- pg$participants[["0086e9c0-418c-404c-8f3c-219de93cc3dc"]]$get_plot(
  mean_line=TRUE,
  cutoff_line=TRUE,
  grapheme_size=4
)
# inspect the plot
example_plot

## ----calc_cons_scores---------------------------------------------------------
mean_cons_scores <- pg$get_mean_consistency_scores( 
  method='euclidean',
  na.rm=TRUE
)

## ----put_in_df----------------------------------------------------------------
participant_ids <- pg$get_ids()
cons_score_df <- data.frame(
  participant_id=participant_ids, 
  mean_cons_score=mean_cons_scores
)
print(head(cons_score_df))

## ----add_num_valid------------------------------------------------------------
num_valid <- pg$get_numbers_all_colored_graphemes()

cons_score_df[['num_allvalid_sounds']] <- num_valid
print(head(cons_score_df))

## ----filter_valid_participants------------------------------------------------
print(paste('number of participants before filtering:', nrow(cons_score_df)))

enough_responses_filter <- cons_score_df$num_allvalid_sounds >= 8
cons_score_df <- cons_score_df[enough_responses_filter, ]

print(paste('number of participants after filtering:', nrow(cons_score_df)))

## ----validate_participant_data------------------------------------------------
validation_df <- pg$check_valid_get_twcv_scores(
  min_complete_graphemes = 8,
  dbscan_eps = 20,
  dbscan_min_pts = 4,
  max_var_tight_cluster = 150,
  max_prop_single_tight_cluster = 0.8,
  safe_num_clusters = 2,
  safe_twcv = 300
)

# again, use filter to only keep data from participants with
# >=8 all-valid responses graphemes
validation_df <- validation_df[enough_responses_filter, ]

cons_score_df[['data_valid']] <- validation_df$valid
cons_score_df[['reason_invalid']] <- validation_df$reason_invalid

# show only participants whose data were classified as invalid,
# and only relevant columns
cons_score_df[!cons_score_df$data_valid, c("participant_id", "mean_cons_score", "reason_invalid")]

## -----------------------------------------------------------------------------
pg$participants[["d47c0e32-e3e2-4acf-84d0-08bf7375308b"]]$get_plot(
  grapheme_size=4
)

## ----cons_score_pdf-----------------------------------------------------------
ggplot(cons_score_df, aes(x=mean_cons_score)) +  
  geom_density() +
  geom_vline(xintercept = 135.3, color="blue") +
  labs(x='Mean consistency score', y='Probability density')

## ----cons_score_ecdf----------------------------------------------------------
ggplot(cons_score_df, aes(x=mean_cons_score)) +
  stat_ecdf(geom = "step") +
  geom_vline(xintercept = 135.3, color="blue") +
  labs(x='Mean consistency score', y='Cumulative proportion of participants')

