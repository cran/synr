#' @title Create a Participant instance.
#' @description Takes in a participant id, set of symbols for which graphemes
#' should be created and participant trial/response data. Returns a Participant
#' instance with all the input data linked to it. For each grapheme, if there
#' are data for less trials than the number specified by n_trials_per_grapheme,
#' NA values are added to affected graphemes' associated vectors of response
#' times/colors.
#' @param participant_id A one-element character vector holding a participant id.
#' @param grapheme_symbols A character vector of symbols/graphemes for which
#' Grapheme instances should be created and linked to the Participant instance.
#' @param n_trials_per_grapheme A one-element numeric vector holding the number
#' of trials per grapheme.
#' @param trial_symbols A character vector that holds one symbol/grapheme
#' for each trial of the participant's consistency test run.
#' @param response_times (optional) A numeric vector. Consistency test times from
#' presentation to response, in order.
#' @param response_colors A character vector. Consistency test response
#' colors, as hex color codes.
#' @param color_space_spec A one-element character vector. What color
#' space is to be used? The following color spaces are supported:
#' "XYZ", "sRGB", "Apple RGB", "Lab", and "Luv"
#' @param test_date (optional) A one-element character vector in the format
#' "YYYY-MM-DD" that indicates on what date the participant
#' finished the consistency test.
#' @examples
#' participant_id <- "1"
#' target_symbols_vec <- c("A", "D", "7")
#' symbol_vec <- c("A", "D", "7",
#'                 "D", "A", "7",
#'                 "7", "A", "D")
#' times_vec <- c(1.1, 0.4, 5,
#'                0.3, 2.4, 7.3,
#'                1, 10.2, 8.4)
#' color_vec <- c("98FF22", "138831", "791322",
#'                "8952FE", "DC8481", "7D89B0",
#'                "001100", "887755", "FF0033")
#' p <- create_participant(participant_id=participant_id,
#'                         grapheme_symbols=target_symbols_vec,
#'                         n_trials_per_grapheme=3,
#'                         trial_symbols=symbol_vec,
#'                         response_times=times_vec,
#'                         response_colors=color_vec,
#'                         color_space_spec="Luv")
#' @export
create_participant <- function(participant_id, grapheme_symbols,
                            n_trials_per_grapheme, trial_symbols,
                            response_times=NULL, response_colors,
                            color_space_spec="Luv",
                            test_date=NULL) {
  new_participant <- Participant$new(id=participant_id)
  if (!is.null(test_date)) {
    new_participant$set_date(test_date)
  }
  for (sym in grapheme_symbols) {
    sym_indices <- which(trial_symbols == sym)
    n_nas <- n_trials_per_grapheme - length(sym_indices)
    if (!is.null(response_times)) {
      sym_response_times <- c(response_times[sym_indices], rep(NA, times=n_nas))
    } else {sym_response_times <- NULL}
    sym_response_colors <- c(response_colors[sym_indices], rep(NA, times=n_nas))
    new_grapheme <- create_grapheme(symbol=sym,
                                    response_times=sym_response_times,
                                    response_colors=sym_response_colors,
                                    color_space_spec=color_space_spec)
    new_participant$add_grapheme(new_grapheme)
  }
  return(new_participant)
}
