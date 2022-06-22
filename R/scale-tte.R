#' @export
scale_tte_y_discrete <- function(..., expand = waiver(), guide = waiver(), position = "left") {
  sc <- discrete_scale(c("y", "ymin", "ymax", "yend", "tte"), "position_d", identity, ..., expand = expand, guide = guide, position = position,
                       super = ScaleDiscretePosition)
  sc$range_c <- ggint$continuous_range()
  structure(sc, class = c('ggtte_scale', class(sc)))
}


# scale_tte_continuous <- function(name = waiver(), breaks = waiver(), minor_breaks = waiver(),
#                                  n.breaks = NULL, labels = waiver(), limits = NULL, expand = waiver(),
#                                  oob = scales::censor, na.value = NA_real_, trans = "identity", guide = waiver(),
#                                  position = "left", sec.axis = waiver()) {
#   sc <- continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final", "ymax_final", "lower", "middle", "upper", "y0", "tte"), "position_c",
#                          identity, name = name, breaks = breaks, n.breaks = n.breaks, minor_breaks = minor_breaks, labels = labels, limits = limits,
#                          expand = expand, oob = oob, na.value = na.value, trans = trans, guide = guide, position = position,
#                          super = ScaleContinuousPosition)
#   sc <- ggplot2:::set_sec_axis(sec.axis, sc)
#   structure(sc, class = c('ggtte_scale', class(sc)))
# }
