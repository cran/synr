test_that("TWCV score is calculated when just one cluster is involved", {
  point_matrix <- matrix(
    c(1, 0, 0, -1, 0, 0),
    ncol=3,
    byrow=TRUE
  )
  cluster_vector <- rep(1, 2)
  expect_equal(
    total_within_cluster_variance(point_matrix, cluster_vector),
    2
  )
})

test_that(
  paste0(
    "error is thrown when number of points and ",
    "cluster assignments do not match"
  ),
  {
  point_matrix <- matrix(
    c(1, 0, 0, -1, 0, 0),
    ncol=3,
    byrow=TRUE
  )
  cluster_vector <- rep(1, 1)
  expect_error(
    total_within_cluster_variance(point_matrix, cluster_vector)
  )
})

test_that(
  paste0(
    "error is thrown when number of clusters matches ",
    "number of points"
  ),
  {
    point_matrix <- matrix(
      c(1, 0, 0, -1, 0, 0),
      ncol=3,
      byrow=TRUE
    )
    cluster_vector <- c(1, 2)
    expect_error(
      total_within_cluster_variance(point_matrix, cluster_vector)
    )
})
