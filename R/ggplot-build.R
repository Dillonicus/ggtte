#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.ggtte <- function (plot)
{
  plot <- ggint$plot_clone(plot)
  if (length(plot$layers) == 0) {
    plot <- plot + geom_blank()
  }
  layers <- plot$layers
  layer_data <- lapply(layers, function(y) y$layer_data(plot$data))
  scales <- plot$scales
  by_layer <- function(f) {
    out <- vector("list", length(data))
    for (i in seq_along(data)) {
      out[[i]] <- f(l = layers[[i]], d = data[[i]])
    }
    out
  }
  data <- layer_data
  data <- by_layer(function(l, d) l$setup_layer(d, plot))
  layout <- create_layout.ggtte(plot$facet, plot$coordinates)
  data <- layout$setup(data, plot$data, plot$plot_env)
  data <- by_layer(function(l, d) l$compute_aesthetics(d,
                                                       plot))
  data <- lapply(data, ggint$scales_transform_df, scales = scales)
  scale_x <- function() scales$get_scales("x")
  scale_y <- function() scales$get_scales("y")
  layout$train_position(data, scale_x(), scale_y())
  data <- layout$map_position(data)
  data <- by_layer(function(l, d) l$compute_statistic(d, layout))
  data <- by_layer(function(l, d) l$map_statistic(d, plot))
  ggint$scales_add_missing(plot, c("x", "y"), plot$plot_env)
  data <- by_layer(function(l, d) l$compute_geom_1(d))
  data <- by_layer(function(l, d) l$compute_position(d, layout))
  layout$reset_scales()
  layout$train_position(data, scale_x(), scale_y())
  layout$setup_panel_params()
  data <- layout$map_position(data)
  npscales <- scales$non_position_scales()
  if (npscales$n() > 0) {
    lapply(data, ggint$scales_train_df, scales = npscales)
    data <- lapply(data, ggint$scales_map_df, scales = npscales)
  }
  data <- by_layer(function(l, d) l$compute_geom_2(d))
  data <- by_layer(function(l, d) l$finish_statistics(d))
  data <- layout$finish_data(data)
  plot$labels$alt <- ggint$get_alt_text(plot)
  structure(list(data = data, layout = layout, plot = plot),
            class = "ggplot_built")
}
