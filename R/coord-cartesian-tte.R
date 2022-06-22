
#'@export
as_ggtteCoord <- function(coord) UseMethod('as_ggtteCoord')

#'@export
as_ggtteCoord.default <- function(coord) {
  abort(glue("No known method to make {class(coord)[1]} ggtte friendly"))
}

#'@export
as_ggtteCoord.CoordCartesian <- function(coord) {
  if(class(coord)[1L] != 'CoordCartesian')   abort(glue("No known method to make {class(coord)[1]} ggtte friendly"))
  ggplot2::ggproto('CoordTTE',
                   CoordTTECartesian,
                   limits = coord$limits,
                   expand = coord$expand,
                   default = coord$default,
                   clip = coord$clip)
}

#'@export
as_ggtteCoord.CoordTTE <- function(coord) {
  coord
}


CoordTTECartesian <- ggplot2::ggproto("CoordTTECartesian", CoordCartesian,
                                      labels =  function (self, labels, panel_params)
                                      {
                                        positions_x <- c("top", "bottom")
                                        # needed to account for separate y-axis labels for each facet panel
                                        positions_y <- c("left", "right", "left", "right")
                                        list(x = lapply(c(1, 2), function(i) {
                                          ggplot2:::panel_guide_label(panel_params$guides, position = positions_x[[i]],
                                                                      default_label = labels$x[[i]])
                                        }), y = lapply(c(1:4), function(i) {
                                          ggplot2:::panel_guide_label(panel_params$guides, position = positions_y[[i]],
                                                                      default_label = labels$y[[i]])
                                        }))
                                      },
                                      setup_panel_guides = function (self, panel_params, guides, params = list())
                                      {
                                        aesthetics <- c("x", "y", "x.sec", "y.sec", "y.risk", "y.risk.sec")
                                        names(aesthetics) <- aesthetics
                                        guides <- lapply(aesthetics, function(aesthetic) {
                                          ggplot2:::resolve_guide(aesthetic, panel_params[[aesthetic]], guides,
                                                                  default = guide_axis(), null = guide_none())
                                        })
                                        guides <- lapply(guides, ggplot2:::validate_guide)
                                        guides <- lapply(aesthetics, function(aesthetic) {
                                          guide <- guides[[aesthetic]]
                                          scale <- panel_params[[aesthetic]]
                                          guide$position <- guide$position %|W|% scale$position
                                          guide
                                        })
                                        panel_params$guides <- guides
                                        panel_params
                                      },
                                      train_panel_guides = function (self, panel_params, layers, default_mapping, params = list())
                                      {
                                        aesthetics <- c("x", "y", "x.sec", "y.sec", "y.risk", "y.risk.sec")
                                        names(aesthetics) <- aesthetics
                                        panel_params$guides <- lapply(aesthetics, function(aesthetic) {
                                          axis <- substr(aesthetic, 1, 1)
                                          guide <- panel_params$guides[[aesthetic]]
                                          guide <- guide_train(guide, panel_params[[aesthetic]])
                                          guide <- guide_transform(guide, self, panel_params)
                                          guide <- guide_geom(guide, layers, default_mapping)
                                          guide
                                        })
                                        panel_params
                                      },
                                      setup_panel_params = function(self, scale_x, scale_y, params = list()) {
                                        c(
                                          ggplot2:::view_scales_from_scale(scale_x, self$limits$x, self$expand),
                                          ggplot2:::view_scales_from_scale(scale_y, self$limits$y, self$expand
                                          )
                                        )
                                      }
)

