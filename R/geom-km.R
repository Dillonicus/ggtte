#' @title Kaplan-Meier plot
#' @export
geom_km <- function(mapping = NULL, data = NULL, stat = "KM", position = "identity",
                    show.legend = NA, inherit.aes = TRUE, na.rm = TRUE, ...) {
  l <- layer(geom = GeomKM,
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          na.rm = na.rm,
          ...
        ))

  l
}

#' @export
GeomKM <- ggproto("GeomKM", GeomStep,
                  draw_group = function(data, scales, coordinates, ...) {
                    path <- transform(data, alpha = NA)
                    GeomPath$draw_panel(path, scales, coordinates)
                  },
                  required_aes = c("x", "y")
)
