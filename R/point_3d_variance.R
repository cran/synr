#' @title Calculate sample variance of 3D point distance from centroid
#'
#' @description Calculates sample variance of points' distances in
#' 3D space from their centroid. This function is normally only used
#' indirectly through `validate_get_twcv`.
#' @section Details:
#' The variance here is taken to mean
#' the sum of variances for each dimension/axis:
#' \deqn{
#' \frac{\sum_{i=1}^n (x_i-x_m)^2 + (y_i-y_m)^2 + (z-z_m)^2}{n-1}
#' }{
#' sum_(i=1)^n ((x - x_m)^2 + (y - y_m)^2 + (z - z_m)^2) / (n - 1)
#' }
#' Where \eqn{X/Y/Z} represent one axis each, \eqn{a_m}{a_m} represents the mean
#' of all points' coordinates on an axis, and \eqn{n} represents the total
#' number of points.

#' @param point_matrix An n-by-3 numerical matrix where each
#' row corresponds to a single point in 3D space.
#' @return A one-element numeric vector holding calculated variance
#' @seealso \code{\link{centroid_3d_sq_dist}}

point_3d_variance <- function(
  point_matrix
) {
  # if there is only one row in the matrix of points,
  # return 0 (since the single point
  # is then itself the centroid, and at distance 0 from itself)
  if (nrow(point_matrix) == 1) {
    return(0)
  }

  dist_var <- centroid_3d_sq_dist(point_matrix) /
    (nrow(point_matrix) - 1)
  return(dist_var)
}
