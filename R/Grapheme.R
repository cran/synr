#' A Reference Class for representing consistency test graphemes
#' 
#' @field symbol A one-element character vector containing the symbol/set of symbols
#' that describe(s) the grapheme, e. g. '7' or 'Monday'. Set at class new() call or using set_symbol method.
#' @field response_colors A matrix where each row specifies color coordinates for each participant
#' response. Set using set_colors method.
#' @field response_times A numeric vector of response times. Set using set_times method.
#' @field color_space A one-element character vector which describes the color space
#' that response colors are coded in. Set when using set_colors method.
#'
#' @importFrom methods new
#' @export Grapheme
#' @exportClass Grapheme
#' @examples
#' a <- synr::Grapheme$new(symbol='a')
#' a$set_colors(c("#101010", NA), "Luv")
#' a$set_times(c(5, 10))
#' a$get_num_non_na_colors()


Grapheme<- setRefClass("Grapheme",
                       fields = list(symbol= "character",
                                     response_colors = "matrix",
                                     response_times = "numeric",
                                     color_space="character"),

                       methods = list(
                         set_colors = function(hex_codes, color_space_spec) {
                           "Set response colors, using passed RGB hex codes. Converts
                           the hex codes to color coordinates in the specified
                           color space. Supports the following color spaces:
                           \"XYZ\", \"sRGB\", \"Apple RGB\", \"Lab\", and \"Luv\".
                           For all NA values passed, a row of NA values will be included
                           in the matrix (preserving order of responses). Returned/set
                           response colors are in the format of a matrix where each
                           row represents one response/color, and each
                           column represents one color coordinate axis (there are always
                           3 axes used for the currently supported color spaces)"
                           color_matrix <- matrix(NA, nrow=length(hex_codes), ncol=3)
                           non_na_indices <- which(!is.na(hex_codes))
                           hex_codes <- hex_codes[non_na_indices]
                           color_matrix[non_na_indices, ] <- convertColor(t(col2rgb(hex_codes))/255,
                                                        from="sRGB",
                                                        to=color_space_spec)
                           response_colors <<- color_matrix
                           color_space <<- color_space_spec
                         },

                         set_times = function(times) {
                           "Add response times, using passed numeric vector."
                           response_times <<- times
                         },

                         set_symbol = function(symbol_chars) {
                           "Set the grapheme's symbol attribute, using a passed
                           one-element character vector."
                           symbol <<- symbol_chars
                         },

                         get_abbreviated_symbol = function() {
                           "Return a short (3 character) representation of
                           the grapheme's symbol."
                           if (!length(symbol)) {
                             stop("get_short_symbol() method called for grapheme without a symbol. Please specify a symbol before using this method.")
                           }
                           return(substr(symbol, 1, 3))
                         },

                         get_num_non_na_colors = function() {
                           "Get the number of response colors that are non-NA, returned as
                           a one-element numeric vector."
                           return(sum(complete.cases(response_colors)))
                         },

                         has_only_non_na_colors = function() {
                           "Returns TRUE if the grapheme only has responses with valid colors,
                           FALSE if there are responses with nonvalid colors or there are
                           no responses at all."
                           if (!nrow(response_colors)) {
                             return(FALSE)
                           }
                           if (nrow(response_colors) > get_num_non_na_colors()) {
                             return(FALSE)
                           }
                           return(TRUE)
                         },

                         get_hex_colors = function() {
                           rgb_colors <- convertColor(response_colors, from=color_space, to="sRGB")
                           non_na_row_indices <- which(rowSums(is.na(rgb_colors)) == 0)
                           num_resp <- nrow(rgb_colors)
                           hex_vec <- rep(NA, num_resp)
                           if (length(non_na_row_indices > 0)) {
                            hex_vec[non_na_row_indices] <- rgb(rgb_colors[non_na_row_indices, , drop=FALSE])
                           }
                           return(hex_vec)
                         },

                         get_consistency_score = function(na.rm=FALSE,
                                                          method = "euclidean") {
                           "Calculate the consistency score based on the
                           Grapheme instance's response colors. Throws an
                           error if no responses have been registered yet.
                           Always returns NA if all grapheme responses are NA.
                           If na.rm=FALSE, returns NA if any grapheme response
                           is NA. If na.rm=TRUE, returns the consistency score
                           for non-NA responses. This function relies on the
                           base/stats function dist() and so supports only
                           distance calculation methods implemented by dist()
                           (use help(dist) to learn more about it)."
                           if (!nrow(response_colors)) {
                             err_mess <- paste0("This grapheme, with symbol ", symbol,
                                                ' has no registered response colors. Please register responses before calling .get_consistency_score().')
                             stop(err_mess)
                           }
                           if (all(is.na(response_colors))) {
                             return(NA)
                           }
                           distances_matrix <- dist(response_colors, method=method)
                           sum_distances <- sum(distances_matrix, na.rm=na.rm)
                           return(sum_distances)
                         },

                         get_mean_color = function(na.rm=FALSE) {
                           "Average all registered response's colors and
                           return the result (using the color space
                           set at grapheme initialization) as a 3-element vector."
                           if (all(is.na(response_colors))) {
                             return(NA)
                           }
                           return(colMeans(response_colors))
                         },

                         get_mean_response_time = function(na.rm=FALSE) {
                           "Get the mean of the grapheme's associated
                           response times."
                           if (!length(response_times)) {
                             err_mess <- paste0("This grapheme, with symbol ", symbol,
                                                ' has no registered response time. Please register responses before calling get_mean_response_time().')
                             stop(err_mess)
                           }
                           if (all(is.na(response_times))) {
                             return(NA)
                           }
                           mean_rtime <- mean(response_times, na.rm=na.rm)
                           return(mean_rtime)
                         },

                         get_plot_data_list = function() {
                           "Get a list of the grapheme's data, bundled up in
                           a format ready for use in Participant.get_plot_data()
                           method as a row of plot
                           data."
                           if (!nrow(response_colors)) {
                             err_mess <- paste0("This grapheme, with symbol ", symbol,
                                                ' has no registered response colors. Please register responses before calling .get_plot_data_row().')
                             stop(err_mess)
                           }
                           color_hexes <- as.character(get_hex_colors())
                           plot_data_list <- list()
                           plot_data_list[[1]] <- as.character(get_abbreviated_symbol())
                           plot_data_list[[2]] <- as.numeric(get_consistency_score())
                           for (hex in color_hexes) {
                             plot_data_list[[length(plot_data_list)+1]] <- hex
                           }
                           return(plot_data_list)
                         }
                       )
)
