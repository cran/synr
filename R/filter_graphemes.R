#' @title Filter graphemes of a single participant
#'
#' @description Takes in a list of Grapheme objects and a character vector.
#' Returns a list of Grapheme objects, consisting of the participant's
#' graphemes which had a symbol included in the character vector to filter by.
#'
#' @param graphemes A list of Grapheme objects.
#' @param symbol_vector A character vector of symbols to filter the
#' participant's graphemes by. Alternatively NULL (default), in which case
#' no filtering will be done and the full grapheme list is returned.
#' @return A list of Grapheme objects.
#' @keywords internal

filter_graphemes <- function(
  graphemes,
  symbol_vector=NULL
) {
  if (is.null(symbol_vector)) {
    return(graphemes)
  }
  grapheme_mask <- sapply(
    graphemes,
    function(grapheme) {
      grapheme$symbol %in% symbol_vector
    }
  )
  filtered_graphemes <- graphemes[grapheme_mask]
  return(filtered_graphemes)
}
