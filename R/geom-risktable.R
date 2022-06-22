#' @title Survival table plot
#' @description
#' Generates a survival table plot.
#' @export
geom_risktable <- function(mapping = NULL, data = NULL, stat = "RiskTable",
                           position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                           table.type = "r", ...) {

  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRiskTable,
    position = position,
    inherit.aes = inherit.aes,
    show.legend = show.legend,
    params = list(
      table.type = table.type,
      na.rm = na.rm,
      ...
    ),
    layer_class = TTELayer
  )

  structure(l, class = c("ggtte_layer", class(l)))
}

#' @title Risk and Event Table
#' @export
GeomRiskTable <- ggplot2::ggproto("GeomRiskTable", ggplot2::GeomText,
                                  draw_panel = function (data, panel_params, coord, parse = FALSE, na.rm = FALSE,
                                                         check_overlap = FALSE)
                                  {
                                    exclude <- c('x', 'y', 'n.risk', 'n.event', 'n.censor', 'estimate', 'std.error', 'conf.high', 'conf.low', 'cum.event', 'cum.censor', 'status', 'label')
                                    aes <- setdiff(names(data), exclude)
                                    aes <- unique(data.table(data)[, .SD, .SDcols = aes])
                                    levels <- unique(data$group)
                                    xbreak <- na.omit(panel_params$x$breaks)
                                    breaks <- data.table::data.table(x = rep(xbreak, length(levels)), group = rep(levels, each = length(xbreak)))
                                    breaks <- breaks[aes, on = .(group)]
                                    data <- data.table::data.table(data)[, .SD, .SDcols = c('group', exclude)][breaks, on = .(group, x), roll = TRUE]
                                    GeomText$draw_panel(data = data, panel_params = panel_params, coord = coord, parse = parse, na.rm = na.rm, check_overlap = check_overlap)
                                  },
                                  required_aes = c("x", "y"),
                                  extra_params = c("table.type")
)

