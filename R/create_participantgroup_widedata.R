#' @title Create a ParticipantGroup instance
#' @description Takes in a data frame of raw consistency test data and returns a
#' ParticipantGroup instance, to which all the relevant data are linked. See the
#' example data frame synr_exampledf_wide_small and its documentation (help(synr_exampledf_wide_small))
#' for information on the format that this function expects data to be in.
#'
#' Participant id and (optional) test date column names are specified with the
#' exact column names used in the data frame passed to the function. Symbol
#' (i. e. grapheme), response color and response time (optional) columns are
#' specified using regular expressions. You can
#' \href{https://r4ds.had.co.nz/strings.html#matching-patterns-with-regular-expressions}{read about regular expressions using R here}
#' if you want to, but basically what you want to do is this:
#' say your columns with response colors are named
#' "chosen_color_001", "chosen_color_002" and so on. You then simply set
#' color_col_regex="chosen_color" when calling this function. The important thing
#' is that you specify a part of the column names that is unique for the type of
#' column you want to indicate. So if your symbol/grapheme columns are named
#' "grapheme_1", "grapheme_2" ... and your participant id column is named
#' "graphparticipant_1" ..., then symbol_col_regex="graph" wouldn't work,
#' but symbol_col_regex="grapheme_" or even symbol_col_regex="graphe" would.
#' @param raw_df A data frame of raw consistency test data.
#' @param n_trials_per_grapheme A one-element numeric vector holding the number
#' of trials per grapheme that was used in the consistency test the data are from.
#' @param participant_col_name A one-element character vector that holds the
#' column name used for the column in raw_df that holds participant id's.
#' (e. g. "participant_id" for the synr::synr_exampledf_wide_small)
#' @param symbol_col_regex A one-element character vector with a regular expression
#' (see above) unique to columns in the passed data frame that hold trial graphemes/symbols.
#' @param color_col_regex A one-element character vector with a regular expression
#' (see above) unique to columns in the passed data frame that hold response color hex codes.
#' @param time_col_regex (optional) A one-element character vector with a regular expression
#' (see above) unique to columns in the passed data frame that hold response times (times
#' from stimulus presentation to response).
#' @param testdate_col_name (optional) A one-element character vector that holds the
#' column name used for the column in raw_df that holds test dates (dates when participants
#' finished the consistency test).
#' @param color_space_spec A one-element character vector. What color
#' space is to be used for analyses of the data? The following color spaces are supported:
#' "XYZ", "sRGB", "Apple RGB", "Lab", and "Luv"
#' @examples
#' pg <- create_participantgroup_widedata(raw_df=synr_exampledf_wide_small,
#'                               n_trials_per_grapheme=2,
#'                               participant_col_name="participant_id",
#'                               symbol_col_regex="symbol",
#'                               color_col_regex="colou*r",
#'                               time_col_regex="response_time",
#'                               color_space_spec="Luv"
#' )
#' cons_means <- pg$get_mean_consistency_scores()
#' print(cons_means)
#' @export
create_participantgroup_widedata <- function(raw_df,
                                    n_trials_per_grapheme=3,
                                    participant_col_name,
                                    symbol_col_regex,
                                    color_col_regex="colou*r",
                                    time_col_regex=NULL,
                                    testdate_col_name=NULL,
                                    color_space_spec="Luv") {
  new_pgroup <- ParticipantGroup$new()
  participant_vector <- as.character(raw_df[[participant_col_name]])
  symbol_col_bool <- grepl(symbol_col_regex, colnames(raw_df))
  symbol_mat <- as.matrix(raw_df[, symbol_col_bool])
  color_col_bool <- grepl(color_col_regex, colnames(raw_df))
  color_mat <- as.matrix(raw_df[, color_col_bool])
  if (!is.null(time_col_regex)) {
    time_col_bool <- grepl(time_col_regex, colnames(raw_df))
    time_mat <- as.matrix(raw_df[, time_col_bool])
  }
  if (!is.null(testdate_col_name)) {
    testdate_vector <- raw_df[[testdate_col_name]]
  }
  unique_symbols <- unique(as.vector(symbol_mat))
  for (row_index in 1:nrow(raw_df)) {
    participant_id <- participant_vector[row_index]
    symbol_vector <- as.vector(symbol_mat[row_index, ])
    color_vector <- as.vector(color_mat[row_index, ])
    if (!is.null(time_col_regex)) {
      time_vector <- as.vector(time_mat[row_index, ])
    } else {time_vector <- NULL}
    if (!is.null(testdate_col_name)) {
      test_date <- testdate_vector[row_index]
    } else {test_date <- NULL}
    new_p <- create_participant(participant_id=participant_id,
                                  grapheme_symbols=unique_symbols,
                                  n_trials_per_grapheme=n_trials_per_grapheme,
                                  trial_symbols=symbol_vector,
                                  response_times=time_vector,
                                  response_colors=color_vector,
                                  color_space_spec=color_space_spec,
                                  test_date=test_date)
    new_pgroup$add_participant(new_p)
  }
  return(new_pgroup)
}
