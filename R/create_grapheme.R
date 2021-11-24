#' @title Create a grapheme instance
#' @description Takes in a symbol/grapheme and sets of response times/colors,
#' then creates a Grapheme instance that holds the passed information and returns it.
#' @param symbol A one-element character vector holding a symbol/grapheme.
#' @param response_times (optional) A numeric vector. Times from presentation to
#' response, in order.
#' @param response_colors A character vector. Response colors, as hex color
#' codes.
#' @param color_space_spec A one-element character vector. What color space
#' is to be used? The following color spaces are supported:
#' "XYZ", "sRGB", "Apple RGB", "Lab", and "Luv"
#' @examples
#' create_grapheme(symbol="a", response_times=c(2.3, 6.7, 0.4),
#' response_colors=c("84AE99", "9E3300", "000000"), color_space_spec="Luv")
#' @export
create_grapheme <- function(symbol, response_times=NULL, response_colors, color_space_spec="Luv") {
  has_no_pound <- (!grepl("^#", response_colors) & !is.na(response_colors) & !is.null(response_colors))
  response_colors[has_no_pound] <- paste0("#", response_colors[has_no_pound])
  new_grapheme <- Grapheme$new(symbol=symbol)
  if (!is.null(response_times)) {
    new_grapheme$set_times(times=response_times)
  }
  new_grapheme$set_colors(hex_codes=response_colors,
                          color_space_spec=color_space_spec)
  return(new_grapheme)
}
