#' Raw consistency test data example, wide format (small)
#'
#' A data frame with an example of raw consistency test data that are
#' compatible with the synr package's `create_participantgroup_widedata` function,
#' with data for three participants from a test that included three
#' graphemes ("A", "D", 7) and two responses
#' per grapheme. More graphemes and/or
#' responses per grapheme can be handled by the package (though participant
#' plots do not function correctly if there are more than three responses
#' per grapheme)
#'
#' @format A data frame with 3 rows and 8 columns:
#' \describe{
#'   \item{participant_id}{Participant ID}
#'   \item{symbol_1}{Column with symbol/grapheme connected to first response}
#'   \item{response_color_1}{Column with color of first response}
#'   \item{response_time_1}{(optional) Column with time from
#'   presentation to response, for first response}
#'   \item{symbol_2}{Column with symbol/grapheme connected to second response}
#'   \item{response_color_2}{Column with color of second response}
#'   \item{response_time_2}{(optional) Column with time from
#'   presentation to response, for second response}
#'   \item{symbol_3}{Column with symbol/grapheme connected to third response}
#'   \item{response_color_3}{Column with color of third response}
#'   \item{response_time_3}{(optional) Column with time from
#'   presentation to response, for third response}
#'   \item{symbol_4}{Column with symbol/grapheme connected to fourth response}
#'   \item{response_color_4}{Column with color of fourth response}
#'   \item{response_time_4}{(optional) Column with time from
#'   presentation to response, for fourth response}
#'   \item{symbol_5}{Column with symbol/grapheme connected to fifth response}
#'   \item{response_color_5}{Column with color of fifth response}
#'   \item{response_time_5}{(optional) Column with time from
#'   presentation to response, for fifth response}
#'   \item{symbol_6}{Column with symbol/grapheme connected to sixth response}
#'   \item{response_color_6}{Column with color of sixth response}
#'   \item{response_time_6}{(optional) Column with time from
#'   presentation to response, for sixth response}
#' }
"synr_exampledf_wide_small"

#' Raw consistency test data example, long format (small)
#'
#' A data frame with an example of raw consistency test data that are
#' compatible with the synr package's `create_participantgroup` function,
#' with completely made updata for three participants from a hypothetical 
#' test that included three graphemes ("A", "D", 7) and two responses
#' per grapheme. More graphemes and/or
#' responses per grapheme can be handled by the package (though participant
#' plots do not function correctly if there are more than three responses
#' per grapheme). Note that response times are optional. If you don't have
#' them, you can still use synr - see `help(create_participantgroup_widedata)`.
#'
#' @format A data frame with 18 rows and 4 columns:
#' \describe{
#'   \item{participant_id}{Participant ID}
#'   \item{trial_symbol}{Column of trial symbols/graphemes}
#'   \item{response_color}{Column of trial response colors}
#'   \item{response_time}{Column of trial response times}
#' }
"synr_exampledf_long_small"

#' Raw consistency test data example, long format (small)
#'
#' A data frame with an example of raw consistency test data that are
#' compatible with the synr package's `create_participantgroup' function.
#' The color and 'symbol' data are from five actual participants who did
#' a test that included all letters, digits and weekdays, with 3 trials
#' per grapheme. The response times are randomly generated. 
#' Note that response times are optional. If you don't have
#' them, you can still use synr - see `help(create_participantgroup_widedata)`.
#'
#' @format A data frame with 516 rows and 4 columns:
#' \describe{
#'   \item{participant_id}{Participant ID}
#'   \item{trial_symbol}{Column of trial symbols/graphemes}
#'   \item{response_color}{Column of trial response colors}
#'   \item{response_time}{Column of trial response times}
#' }
"synr_exampledf_large"
