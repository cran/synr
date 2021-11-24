## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6,
  fig.height=5
)

## ----load_synr----------------------------------------------------------------
library(synr)

## ----load_vignette_build_packages, include = FALSE----------------------------
library(dbscan)
library(plotly)

## ----plotly_convenience_functions, include = FALSE----------------------------
plot_color_clusters <- function(
  color_matrix, 
  cluster_vector,
  grapheme_vector=NULL,
  ax_min = 0, ax_max = 1
) {
  if (!is.null(grapheme_vector)) {
    fig <- plot_ly(
      x=color_matrix[, 1], # first color axis values
      y=color_matrix[, 2], # second color axis values
      z=color_matrix[, 3], # third color axis values
      type="scatter3d", # use a 3D scatter plot
      mode="markers", # plot just points by themselves (rather than eg lines)
      color=cluster_vector, # color points according to which cluster they were assigned to
      customdata = grapheme_vector, # include graphemes data
      hovertemplate = 'Axis 1: %{x}<br>Axis 2: %{y}<br>Axis 3: %{z}<br>Grapheme: %{customdata}' # define a template for mouse hover info
    )
  } else {
    fig <- plot_ly(
      x=color_matrix[, 1], # first color axis values
      y=color_matrix[, 2], # second color axis values
      z=color_matrix[, 3], # third color axis values
      type="scatter3d", # use a 3D scatter plot
      mode="markers", # plot just points by themselves (rather than eg lines)
      color=cluster_vector, # color points according to which cluster they were assigned to
      hovertemplate = 'Axis 1: %{x}<br>Axis 2: %{y}<br>Axis 3: %{z}<br>' # define a template for mouse hover info
    ) 
  }
  

  # make axis titles more descriptive
  axis_title_font <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#4f4f4f"
  )
  x_axis_layout <- list(
    title = list(
      text = "Axis 1",
      font = axis_title_font
    ),
    range = c(ax_min, ax_max)
  )
  y_axis_layout <- list(
    title = list(
      text = "Axis 2",
      font = axis_title_font
    ),
    range = c(ax_min, ax_max)
  )
  z_axis_layout <- list(
    title = list(
      text = "Axis 3",
      font = axis_title_font
    ),
    range = c(ax_min, ax_max)
  )
  fig <- fig %>% layout(
    scene = list(
      xaxis = x_axis_layout, 
      yaxis = y_axis_layout,
      zaxis = z_axis_layout
    )
  ) %>% 
    partial_bundle()
  return(fig)
}



plot_actual_colors <- function(
  color_matrix,
  hex_codes,
  grapheme_vector = NULL,
  ax_min = 0,
  ax_max = 1
) {
  if (!is.null(grapheme_vector)) {
    fig <- plot_ly(
      x=color_matrix[, 1], # first color axis values
      y=color_matrix[, 2], # second color axis values
      z=color_matrix[, 3], # third color axis values
      type="scatter3d", # use a 3D scatter plot
      mode="markers", # plot just points by themselves (rather than eg lines)
    customdata = grapheme_vector, # include graphemes data
    marker = list(color = hex_codes),
    hovertemplate = 'Axis 1: %{x}<br>Axis 2: %{y}<br>Axis 3: %{z}<br>Grapheme: %{customdata}' # define a template for mouse hover info
    )
  } else {
    fig <- plot_ly(
      x=color_matrix[, 1], # first color axis values
      y=color_matrix[, 2], # second color axis values
      z=color_matrix[, 3], # third color axis values
      type="scatter3d", # use a 3D scatter plot
      mode="markers", # plot just points by themselves (rather than eg lines)
      marker = list(color = hex_codes),
      hovertemplate = 'Axis 1: %{x}<br>Axis 2: %{y}<br>Axis 3: %{z}<br>' # define a template for mouse hover info
    )
  }

  # make axis titles more descriptive
  axis_title_font <- list(
    family = "Courier New, monospace",
    size = 18,
    color = "#4f4f4f"
  )
  x_axis_layout <- list(
    title = list(
      text = "Axis 1",
      font = axis_title_font
    ),
    range = c(ax_min, ax_max)
  )
  y_axis_layout <- list(
    title = list(
      text = "Axis 2",
      font = axis_title_font
    ),
    range = c(ax_min, ax_max)
  )
  z_axis_layout <- list(
    title = list(
      text = "Axis 3",
      font = axis_title_font
    ),
    range = c(ax_min, ax_max)
  )
  fig <- fig %>% layout(
    scene = list(
      xaxis = x_axis_layout, 
      yaxis = y_axis_layout,
      zaxis = z_axis_layout
    )
  ) %>% 
    partial_bundle()
  return(fig)
}

## ----echo=FALSE---------------------------------------------------------------
p <- Participant$new()
g <- Grapheme$new(symbol='A')
g$set_colors(c("#FF0000", "#000000", "#000000"), "Luv")
p$add_grapheme(g)
for (l in LETTERS[2:length(LETTERS)]) {
  g <- Grapheme$new(symbol=l)
  g$set_colors(c("#FF0000", "#FF0000", "#FF0000"), "Luv")
  p$add_grapheme(g)
}
p$get_plot()

## ----echo=FALSE---------------------------------------------------------------
# helper function for generating random colors
get_random_color <- function() {
  r_val <- runif(1, 0, 1)
  g_val <- runif(1, 0, 1)
  b_val <- runif(1, 0, 1)
  alpha_val <- runif(1, 0, 1)
  hex_val <- rgb(r_val, g_val, b_val, alpha_val)
  return(hex_val)
}

p <- Participant$new(id="1")
for (l in LETTERS) {
  g <- Grapheme$new(symbol=l)
  g$set_colors(c(get_random_color(), get_random_color(), get_random_color()), "Luv")
  p$add_grapheme(g)
}
p$get_plot()

## ----echo=FALSE---------------------------------------------------------------
# helper function
get_bluegreenish_color <- function(green_val) {
  hex_val <- rgb(0, green_val, 1, 1)
  return(hex_val)
}

p <- Participant$new(id="1")
green_vals <- seq(0, 1, length.out = length(LETTERS) * 3)
val_counter <- 1
for (l in LETTERS) {
  g <- Grapheme$new(symbol=l)
  g$set_colors(
    c(
      get_bluegreenish_color(green_vals[val_counter]),
      get_bluegreenish_color(green_vals[val_counter + 1]),
      get_bluegreenish_color(green_vals[val_counter + 2])
    ),
    "Luv"
  )
  p$add_grapheme(g)
  val_counter <- val_counter + 3
}
p$get_plot()

## ----echo=FALSE---------------------------------------------------------------
p <- Participant$new(id="1")
green_vals <- seq(0.65, 0.8, length.out = length(LETTERS) * 3)
val_counter <- 1
for (l in LETTERS) {
  g <- Grapheme$new(symbol=l)
  g$set_colors(
    c(
      get_bluegreenish_color(green_vals[val_counter]),
      get_bluegreenish_color(green_vals[val_counter + 1]),
      get_bluegreenish_color(green_vals[val_counter + 2])
    ),
    "Luv"
  )
  p$add_grapheme(g)
  val_counter <- val_counter + 3
}
p$get_plot()

## ----echo=FALSE---------------------------------------------------------------
# helper function for generating random bright colors
get_random_color_bright_color <- function() {
  r_val <- runif(1, 0.3, 1)
  g_val <- runif(1, 0.3, 1)
  b_val <- runif(1, 0.3, 1)
  hex_val <- rgb(r_val, g_val, b_val, 1)
  return(hex_val)
}
p <- Participant$new(id="1")
for (dig in as.character(0:9)) {
  g <- Grapheme$new(symbol=dig)
  g$set_colors(
    c(
      get_random_color_bright_color(),
      get_random_color_bright_color(),
      get_random_color_bright_color()
    ),
    "sRGB"
  )
  p$add_grapheme(g)
}
color_mat <- p$get_nonna_color_resp_mat()
hex_vals <- apply(color_mat, 1, function(x) {rgb(x[1], x[2], x[3])})

mid_point <- apply(color_mat, 2, function(x) {mean(x)})
mid_hex <- rgb(0, 0, 0)

plot_actual_colors(
  rbind(color_mat, mid_point),
  c(hex_vals, mid_hex),
  c(rep(0:9, each=3), 'MIDDLE')
)

## ----echo=FALSE---------------------------------------------------------------
p_dbscan_ex <- Participant$new(id="1")
resp_colors <- c(
  '#FFFFFF', '#050505', '#FAFAFA',
  '#FF0000', '#151515', '#EE1100',
  '#00DD22', '#DFDFDF', '#CCCC00',
  '#3300EE', '#FBFBFB', '#FDFEFF',
  '#FE0505', '#FD2233', '#333333',
  '#EEEEEE', '#222222', '#F0F0F0',
  '#EE5555', '#FA0055', '#FE0344'
)
resp_counter <- 1
for (dig in as.character(0:6)) {
  g <- Grapheme$new(symbol=dig)
  g$set_colors(
    c(
      resp_colors[resp_counter],
      resp_colors[resp_counter+1],
      resp_colors[resp_counter+2]
    ),
    "sRGB"
  )
  p_dbscan_ex$add_grapheme(g)
  resp_counter <- resp_counter + 3
}
color_mat_dbscan_ex <- p_dbscan_ex$get_nonna_color_resp_mat()
hex_vals_dbscan_ex <- apply(color_mat_dbscan_ex, 1, function(x) {rgb(x[1], x[2], x[3])})

plot_actual_colors(
  color_mat_dbscan_ex,
  hex_vals_dbscan_ex,
  rep(0:6, each=3)
)

## ----echo=FALSE---------------------------------------------------------------
dbscan_res_dbscan_ex <- dbscan(color_mat_dbscan_ex, eps = 0.15, minPts = 3)

plot_color_clusters(
  color_mat_dbscan_ex,
  dbscan_res_dbscan_ex$cluster,
  rep(0:6, each=3)
)

## ----echo=FALSE---------------------------------------------------------------
get_bluereddish_color <- function(red_val) {
  hex_val <- rgb(red_val, 0, 1, 1)
  return(hex_val)
}

p <- Participant$new(id="1")
letts1 <- LETTERS[1:6]
letts2 <- LETTERS[7:12]

green_vals <- seq(0, 1, length.out = length(letts1) * 3)
red_vals <- seq(0, 1, length.out = length(letts1) * 3)
val_counter <- 1
for (l in letts1) {
  g <- Grapheme$new(symbol=l)
  g$set_colors(
    c(
      get_bluegreenish_color(green_vals[val_counter]),
      get_bluegreenish_color(green_vals[val_counter + 1]),
      get_bluegreenish_color(green_vals[val_counter + 2])
    ),
    "sRGB"
  )
  p$add_grapheme(g)
  val_counter <- val_counter + 3
}

red_vals <- seq(0, 1, length.out = length(letts1) * 3)
val_counter <- 1
for (l in letts2) {
  g <- Grapheme$new(symbol=l)
  g$set_colors(
    c(
      get_bluereddish_color(green_vals[val_counter]),
      get_bluereddish_color(green_vals[val_counter + 1]),
      get_bluereddish_color(green_vals[val_counter + 2])
    ),
    "sRGB"
  )
  p$add_grapheme(g)
  val_counter <- val_counter + 3
}

color_mat <- p$get_nonna_color_resp_mat()
hex_vals <- apply(color_mat, 1, function(x) {rgb(x[1], x[2], x[3])})

plot_actual_colors(
  color_mat,
  hex_vals,
  c(rep(letts1, each=3), rep(letts2, each=3))
)

## -----------------------------------------------------------------------------
# if you don't already have synr loaded, run 'library(synr)' first
pg <- create_participantgroup(
  raw_df=synr_exampledf_large,
  n_trials_per_grapheme=3,
  id_col_name="participant_id",
  symbol_col_name="trial_symbol",
  color_col_name="response_color",
  time_col_name="response_time", # optional, not necessary for core functionality
  color_space_spec="Luv"
)

## -----------------------------------------------------------------------------
set.seed(1)
val_df <- pg$check_valid_get_twcv_scores(
  min_complete_graphemes = 4,
  dbscan_eps = 20,
  dbscan_min_pts = 4,
  max_var_tight_cluster = 150,
  max_prop_single_tight_cluster = 0.6,
  safe_num_clusters = 3,
  safe_twcv = 250,
  complete_graphemes_only = TRUE,
  symbol_filter = LETTERS
)

head(val_df)

## -----------------------------------------------------------------------------
val_id_df <- cbind(list(participant_id=pg$get_ids()), val_df)

head(val_id_df)

