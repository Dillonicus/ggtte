create_layout.ggtte <- function (facet = FacetNull, coord = CoordCartesian)
{
  ggproto(NULL, LayoutTTE, facet = facet, coord = coord)
}


LayoutTTE <- ggproto("LayoutTTE", Layout,

                     ylabel = function (self, labels)
                     {
                       primary <- self$panel_scales_y[[1]]$name %|W|% labels$y
                       primary <- self$panel_scales_y[[1]]$make_title(primary)
                       secondary <- if (is.null(self$panel_scales_y[[1]]$secondary.axis)) {
                         waiver()
                       }
                       else {
                         self$panel_scales_y[[1]]$sec_name()
                       } %|W|% labels$sec.y
                       if (inherits(secondary, "derived"))
                         secondary <- primary
                       secondary <- self$panel_scales_y[[1]]$make_sec_title(secondary)

                       risk.primary <- self$panel_scales_y[[2]]$name %|W|% labels$y.risk
                       risk.primary <- self$panel_scales_y[[2]]$make_title(risk.primary)

                       risk.secondary <- if(is.null(self$panel_scales_y[[2]]$secondary.axis)) {
                         waiver()
                       }
                       else {
                         self$panel_scales_y[[2]]$sec_name()
                       } %|W|% labels$sec.y.risk
                       if(inherits(risk.secondary, "derived"))
                         risk.secondary <- risk.primary
                       risk.secondary <- self$panel_scales_y[[2]]$make_sec_title(risk.secondary)
                       out <- list(primary = primary, secondary = secondary, risk.primary = risk.primary, risk.secondary = risk.secondary)
                       axis_order <- function(self) {
                         ord <- c("primary", "secondary", "risk.primary", "risk.secondary")
                         if(self$position %in% c("right", "bottom")) {
                           ord <- rev(ord)
                         }
                         ord
                       }
                       out[axis_order(self$panel_scales_y[[1]])]
                     },
                     render_labels = function(self, labels, theme) {
                       x_label_grobs <- lapply(c(1, 2), function(i) {
                         modify <- if(i == 1) {'.top'} else {'.bottom'}

                         if(is.null(labels[['x']][[i]]) || is.waive(labels[['x']][[i]]))
                           return(zeroGrob())
                         element_render(theme = theme, element = paste0("axis.title.x", modify), label = labels[["x"]][[i]],
                                        margin_x = FALSE, margin_y=TRUE)
                       })

                       y_label_grobs <- lapply(c(1:4), function(i) {
                         modify <- if(i %% 2 == 1) {'.left'} else {'.right'}
                         panel <- if(i < 3 ) {''} else {'.risk'}

                         if(is.null(labels[['y']][[i]]) || is.waive(labels[['y']][[i]]))
                           return(zeroGrob())
                         element_render(theme = theme, element = paste0("axis.title.y", modify, panel), label = labels[["y"]][[i]],
                                        margin_x = TRUE, margin_y=FALSE)
                       })
                        label_grobs <- list(
                          x_label_grobs,
                          y_label_grobs
                        )
                        names(label_grobs) <- names(labels)
                        label_grobs
                     })



