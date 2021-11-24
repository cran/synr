test_that("fewer than dbscan_min_pts points data leads to categorization as invalid", {
  point_matrix <- matrix(
    c(1, 0, 0, -1, 0, 0),
    ncol=3,
    byrow=TRUE
  )
  res <- validate_get_twcv(
    point_matrix,
    dbscan_eps = 30,
    dbscan_min_pts = 4,
    max_var_tight_cluster = 100,
    max_prop_single_tight_cluster = 0.6,
    safe_num_clusters = 4,
    safe_twcv = 250
  )
  expect_false(
    res$valid
  )
  expect_equal(
    res$reason_invalid,
    "too_few_color_responses"
  )
  expect_equal(
    res$twcv,
    NA
  )
})


test_that("Data in single tight-knut cluster are classified as invalid", {
  point_matrix <- matrix(
    c(
      1, 0, 0,
      -1, 0, 0,
      0, 0, 0,
      0, 0, 0,
      0, 0, 0,
      0, 0, 0
    ),
    ncol=3,
    byrow=TRUE
  )
  res <- validate_get_twcv(
    point_matrix,
    dbscan_eps = 30,
    dbscan_min_pts = 4,
    max_var_tight_cluster = 100,
    max_prop_single_tight_cluster = 0.6,
    safe_num_clusters = 4,
    safe_twcv = 250
  )
  expect_false(
    res$valid
  )
  expect_equal(
    res$reason_invalid,
    "hi_prop_tight_cluster"
  )
  expect_equal(
    res$twcv,
    0.4
  )
})

test_that("Data in two low-variance clusters are classified as invalid", {
  point_matrix <- matrix(
    c(
      1, 0, 0,
      -1, 0, 0,
      0, 0, 0,
      0, 0, 0,
      100, 100, 100,
      100, 100, 100,
      100, 100, 101,
      100, 100, 100
    ),
    ncol=3,
    byrow=TRUE
  )
  res <- validate_get_twcv(
    point_matrix,
    dbscan_eps = 30,
    dbscan_min_pts = 4,
    max_var_tight_cluster = 100,
    max_prop_single_tight_cluster = 0.6,
    safe_num_clusters = 4,
    safe_twcv = 250
  )
  expect_false(
    res$valid
  )
  expect_equal(
    res$reason_invalid,
    "few_clusters_low_twcv"
  )
  expect_lt(
    abs(res$twcv-0.458),
    0.001
  )
})

test_that("Data in single high-variance cluster are classified as valid", {
  point_matrix <- matrix(
    c(
      0, 0, 0,
      29, 0, 0,
      -29, 0, 0,
      0, 29, 0,
      0, 50, 0,
      0, 70, 0,
      10, 70, 10,
      25, 70, 10
    ),
    ncol=3,
    byrow=TRUE
  )
  res <- validate_get_twcv(
    point_matrix,
    dbscan_eps = 30,
    dbscan_min_pts = 4,
    max_var_tight_cluster = 100,
    max_prop_single_tight_cluster = 0.6,
    safe_num_clusters = 4,
    safe_twcv = 250
  )
  expect_true(
    res$valid
  )
  expect_equal(
    res$reason_invalid,
    ""
  )
  expect_lt(
    abs(res$twcv-521),
    1
  )
})



test_that(
  "Data in two low-variance clusters and having an extra one noise point are
  classified as invalid (since noise cluster needs to have > dbscan_min_pts
  points in order to count toward 'cluster tally') when 'safe_num_clusters'
  is set to 3.", {
  point_matrix <- matrix(
    c(
      1, 0, 0,
      -1, 0, 0,
      0, 0, 0,
      0, 0, 0,
      100, 100, 100,
      100, 100, 100,
      100, 100, 101,
      100, 100, 100,
      50, 50, 50
    ),
    ncol=3,
    byrow=TRUE
  )
  res <- validate_get_twcv(
    point_matrix,
    dbscan_eps = 30,
    dbscan_min_pts = 4,
    max_var_tight_cluster = 100,
    max_prop_single_tight_cluster = 0.6,
    safe_num_clusters = 3,
    safe_twcv = 250
  )
  expect_false(
    res$valid
  )
  expect_equal(
    res$reason_invalid,
    "few_clusters_low_twcv"
  )
  expect_lt(
    abs(res$twcv-0.458),
    0.001
  )
  expect_equal(
    res$num_clusters,
    2
  )
})
