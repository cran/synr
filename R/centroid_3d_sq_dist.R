#' @title Calculate sum of squared 3D point distances from centroid
#'
#' @description Calculates sum of squared point distances in
#' 3D space betweeen points and their centroid.
#' \deqn{
#' \frac{\sum_{i=1}^n (x_i-x_m)^2 + (y_i-y_m)^2 + (z-z_m)^2}
#' }{
#' sum_(i=1)^n ((x - x_m)^2 + (y - y_m)^2 + (z - z_m)^2)
#' }
#' Where \eqn{X/Y/Z} represent one axis each, \eqn{a_m} represents the mean
#' of all points' coordinates on an axis, and \eqn{n} represents the total
#' number of points.

#' @param point_matrix An n-by-3 numerical matrix where each
#' row corresponds to a single point in 3D space.
#' @keywords internal

centroid_3d_sq_dist <- function(
  point_matrix
) {
  # if there is only one row in the matrix of points,
  # return 0 (since the single point
  # is then itself the centroid, and at distance 0 from itself)
  if (nrow(point_matrix) == 1) {
    return(0)
  }
  # calculate mean coordinates to form a 1-by-3 matrix
  # (a row vector with 3 entries), corresponding to the
  # centroid ('mean point')
  mean_point <- apply(point_matrix, 2, mean)
  # for each row of actual coordinate points:
  # 1. subtract the `mean_point` row vector
  # 2. square each of the resulting vector elements
  # 3. sum the resulting vector elements
  point_sq_distances <- apply(
    point_matrix,
    1,
    function(row_point) {
      sum((row_point - mean_point) ** 2)
    }
  )
  sum_sq_distances <- sum(point_sq_distances)
  return(sum_sq_distances)
}
