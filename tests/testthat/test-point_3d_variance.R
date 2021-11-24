test_that(
  paste0("point_3d_variance correctly calculates 3D point distance",
  "from centroid sample variance for simple input"), {
  point_matrix <- matrix(
    c(1, 0, 0, -1, 0, 0),
    ncol=3,
    byrow=TRUE
  )
  expect_equal(
    point_3d_variance(point_matrix),
    2
  )
})


test_that("avg_distance_from_centroid correctly calculates average distance for input based on multiple colors", {
  test_hexes <- c(
    "#FAFAFE",
    "#000000",
    "#ABDC07",
    "#0088BB",
    "#FFFFFF"
  )
  test_color_matrix <- t(sapply(
    test_hexes, function(x) col2rgb(x)
  ))
  expect_lt(
    abs(point_3d_variance(test_color_matrix) - 44311),
    0.2
    # abs(point_3d_variance(test_color_matrix)-35449),
    # 0.4
  )
})
