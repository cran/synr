#' @title Create a ParticipantGroup instance using long-format data
#' @description Takes in a data frame of raw
#' \href{https://stefvanbuuren.name/fimd/sec-longandwide.html}{'long format'}
#' consistency test data and returns a
#' ParticipantGroup instance, to which all the relevant data are linked. See the
#' example data frame `synr_exampledf_long_small` and its documentation
#' (`help(synr_exampledf_long_small)`) for more information on the format
#' that this function expects data to be in.
#'
#' @param raw_df A data frame of 'long format' raw consistency test data.
#' @param n_trials_per_grapheme A one-element numeric vector holding the number
#' of trials per grapheme that was used in the consistency test the data are from.
#' @param id_col_name A one-element character vector that holds the
#' name of the participant id column in raw_df.
#' @param symbol_col_name A one-element character vector that holds the
#' name of the grapheme/symbol column in raw_df.
#' @param color_col_name A one-element character vector that holds the
#' name of the response color (hex codes) column in raw_df.
#' @param time_col_name (optional) A one-element character vector that holds the
#' name of the response time (time from stimulus presentation to response) column
#' in raw_df.
#' @param color_space_spec A one-element character vector specifying which color
#' space to use for calculations with participant data. One of
#' "XYZ", "sRGB", "Apple RGB", "Lab", and "Luv".
#' @examples
#' pg <- create_participantgroup(
#'   raw_df=synr_exampledf_long_small,
#'   n_trials_per_grapheme=2,
#'   id_col_name="participant_id",
#'   symbol_col_name="trial_symbol",
#'   color_col_name="response_color",
#'   time_col_name="response_time",
#'   color_space_spec="Luv"
#' )
#' cons_means <- pg$get_mean_consistency_scores()
#' print(cons_means)
#' @export
create_participantgroup <- function(
  raw_df,
  n_trials_per_grapheme=3,
  id_col_name,
  symbol_col_name,
  color_col_name,
  time_col_name=NULL,
  color_space_spec="Luv"
) {
  new_pgroup <- ParticipantGroup$new()
  id_vector <- as.character(raw_df[[id_col_name]])
  unique_ids <- unique(id_vector)

  symbol_vector <- as.character(raw_df[[symbol_col_name]])
  unique_symbols <- unique(symbol_vector)

  color_vector <- as.character(raw_df[[color_col_name]])

  if (!is.null(time_col_name)) {
    time_vector <- as.numeric(raw_df[[time_col_name]])
  }

  for (part_id in unique_ids) {
    part_rows_mask <- id_vector == part_id
    part_sym_vec <- symbol_vector[part_rows_mask]
    part_col_vec <- color_vector[part_rows_mask]

    if (!is.null(time_col_name)) {
      part_tim_vec <- time_vector[part_rows_mask]
    } else { part_tim_vec <- NULL }

    new_p <- create_participant(participant_id=part_id,
                                grapheme_symbols=unique_symbols,
                                n_trials_per_grapheme=n_trials_per_grapheme,
                                trial_symbols=part_sym_vec,
                                response_times=part_tim_vec,
                                response_colors=part_col_vec,
                                color_space_spec=color_space_spec,
                                test_date=NULL)
    new_pgroup$add_participant(new_p)
  }
  return(new_pgroup)
}
