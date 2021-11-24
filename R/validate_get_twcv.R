#' @title Check if color data are valid and get TWCV
#'
#' @description Checks if passed color data are valid, i. e. are bountiful
#' and varied enough according to passed validation criteria. This function
#' is normally only used indirectly through
#' `Participant$check_valid_get_twcv()` or `ParticipantGroup$get_valid_twcv()`.
#' @section Details:
#' This function relies heavily on the DBSCAN algorithm and its implementation
#' in the R package `dbscan`, for clustering color points. For further
#' information regarding the 'dbscan_eps' and
#' 'dbscan_min_pts' parameters as well as DBSCAN itself, please see
#' the `dbscan` documentation. Once clustering is done, passed validation
#' criteria are applied:
#' \itemize{
#'  \item If too high a proportion of all color points (cut-off specified with
#' `max_prop_single_tight_cluster`) fall within a single 'tight-knit' cluster
#' (with a cluster variance less than or equal to `max_var_tight_cluster`),
#' then the data are always classified as invalid.
#' \item If the first criterion is cleared, \emph{and} points form more than
#' `safe_num_cluster` clusters, data are always classified as valid.
#' \item If the first criterion is cleared, \emph{and} the Total Within-Cluster
#' Variance (TWCV) score is greater than or equal to `safe_twcv`, data are
#' always classified as valid.
#' }
#' Note that this means data can be classified as valid by either having
#' at least 'safe_num_cluster' clusters, \emph{or} by having points composing
#' a smaller number of clusters but spaced relatively far apart
#' \emph{within} these clusters.
#' 
#' The DBSCAN 'noise' cluster only counts towards the 'cluster tally' 
#' (compared with 'safe_num_cluster') if it includes at least 'dbscan_min_pts' points.
#' Points in the noise cluster are however always included in
#' other calculations, e. g. total within-cluster variance (TWCV).
#'
#' @param color_matrix An n-by-3 numerical matrix where each
#' row corresponds to a single point in 3D color space.
#' @param dbscan_eps One-element numerical vector: radius of
#' ‘epsilon neighborhood’ when applying DBSCAN clustering.
#' @param dbscan_min_pts One-element numerical vector:
#' Minimum number of points required in the epsilon neighborhood
#' for core points (including the core point itself).
#' @param max_var_tight_cluster One-element numerical vector:
#' maximum variance for a cluster to be considered 'tight-knit'.
#' @param max_prop_single_tight_cluster One-element numerical vector:
#' maximum proportion of points allowed to be within a 'tight-knit' cluster
#' (if this threshold is exceeded, the data are categorized as invalid).
#' @param safe_num_clusters One-element numerical vector: minimum number of
#' clusters that guarantees validity if points are 'non-tight-knit'.
#' @param safe_twcv One-element numerical vector: minimum total
#' within-cluster variance (TWCV) score that guarantees validity if
#' points are 'non-tight-knit'.
#' @return A list with components
#'  \item{valid}{One-element logical vector}
#'  \item{reason_invalid}{One-element character vector, empty if valid is TRUE}
#'  \item{twcv}{One-element numeric (or NA if can't be calculated) vector,
#' indicating TWCV}
#'  \item{num_clusters}{One-element numeric (or NA if can't be calculated)
#' vector, indicating the number of identified clusters counting toward the
#' tally compared with 'safe_num_clusters'}
#'
#' @seealso \code{\link{point_3d_variance}} for single-cluster variance,
#' \code{\link{total_within_cluster_variance}} for TWCV.

validate_get_twcv <- function(
  color_matrix,
  dbscan_eps = 20,
  dbscan_min_pts = 4,
  max_var_tight_cluster = 150,
  max_prop_single_tight_cluster = 0.6,
  safe_num_clusters = 3,
  safe_twcv = 250
) {
  # if there are less than dbscan_min_pts points, it doesn't make sense to run
  # DBSCAN
  if (nrow(color_matrix) < dbscan_min_pts) {
    return(list(
      valid = FALSE,
      reason_invalid = "too_few_color_responses",
      twcv = NA,
      num_clusters = NA
    ))
  }

  dbscan_res <- dbscan::dbscan(
    color_matrix,
    eps = dbscan_eps,
    minPts = dbscan_min_pts
  )
  cluster_numbers <- unique(dbscan_res$cluster)
  twcv <- total_within_cluster_variance(color_matrix, dbscan_res$cluster)

  # check if the noise cluster consists of at least one but 
  # less than 'dbscan_min_pts' points,
  # in which case it should not be included in the 'cluster tally'
  num_noise_pts <- sum(dbscan_res$cluster == 0)
  noise_c_few_pts <- (num_noise_pts > 0) && (num_noise_pts < dbscan_min_pts)
  num_clusters <- length(cluster_numbers) - as.numeric(noise_c_few_pts)

  for (clu_n in cluster_numbers) {
    cluster_mask <- dbscan_res$cluster == clu_n
    num_in_cluster <- length(dbscan_res$cluster[cluster_mask])
    prop_in_cluster <- num_in_cluster / length(dbscan_res$cluster)
    if (prop_in_cluster > max_prop_single_tight_cluster) {
      # calculate within-cluster variance by passing in only
      # color response data associated with the current cluster
      cluster_var <- point_3d_variance(color_matrix[cluster_mask, ])
      if (cluster_var <= max_var_tight_cluster) {
        return(list(
          valid = FALSE,
          reason_invalid = "hi_prop_tight_cluster",
          twcv = twcv,
          num_clusters = num_clusters
        ))
      }
    }
  }
  # check if TWCV score is low and
  # there are few clusters
  twcv_score <- total_within_cluster_variance(
    color_matrix,
    dbscan_res$cluster
  )
  few_clusters <- num_clusters < safe_num_clusters
  low_twcv <- twcv_score < safe_twcv

  if (few_clusters && low_twcv) {
    return(list(
      valid = FALSE,
      reason_invalid = "few_clusters_low_twcv",
      twcv = twcv,
      num_clusters = num_clusters
    ))
  }

  return(list(
    valid = TRUE,
    reason_invalid = "",
    twcv = twcv,
    num_clusters = num_clusters
  ))
}
