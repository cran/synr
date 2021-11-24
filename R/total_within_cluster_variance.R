#' @title Calculate Total Within Cluster Variance of 3D points
#'
#' @description Calculates \emph{Total Within Cluster Variance(TWCV)} of
#' 3D points. This function is normally only used
#' indirectly through `validate_get_twcv`.
#' @section TWCV:
#' TWCV is a synr-specific term for a measure that aims to describe spread
#' of points in 3D space while taking into account that points belong
#' to distinct clusters.
#' TWCV is calculated in a multi-step process:
#' \enumerate{
#'  \item Each cluster's centroid is calculated.
#'  \item All points' squared distances to their corresponding centroids are
#' calculated.
#'  \item The point-to-centroid squared distances are summed up.
#'  \item The sum of squared distances is divided by the total number
#' of points, minus the number of clusters (to account for decreased
#' degrees of freedom).
#' }

#' @param point_matrix An n-by-3 numerical matrix where each
#' row corresponds to a single point in 3D space.
#' @param cluster_vector A numerical vector of cluster assignments, of
#' length n (ie one assignment per point).
#' @return A one-element numeric vector holding calculated variance
#' @seealso \code{\link{centroid_3d_sq_dist}}

total_within_cluster_variance <- function(
  point_matrix,
  cluster_vector
) {
  num_points <- nrow(point_matrix)
  num_clusters <- length(unique(cluster_vector))
  if (length(cluster_vector) != num_points) {
    stop(
      "The number of cluster assignments does not match the number of points."
    )
  }

  # throw error if there are as many or more clusters as there are points,
  # since this would otherwise cause division by zero error (or
  # negative return value) later
  if (num_clusters >= num_points) {
    stop(paste0(
      "Specifications for more clusters than points were passed. The number ",
      "of clusters must be strictly less than the number of points."
    ))
  }

  # vector for storing per-cluster sum of squared distances from centroid
  cluster_sums <- numeric()
  for (cluster_n in min(cluster_vector):max(cluster_vector)) {
    one_cluster <- point_matrix[cluster_vector == cluster_n, , drop = FALSE]
    sum_distances <- centroid_3d_sq_dist(one_cluster)
    cluster_sums <- c(cluster_sums, sum_distances)
  }
  tot_within_cluster_variance <- sum(cluster_sums) / (num_points - num_clusters)
  return(tot_within_cluster_variance)
}
