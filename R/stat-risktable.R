
#' @export
stat_risktable <- function(mapping = NULL, data = NULL, stat = "RiskTable", geom = "text",
                           position = "identity",
                           show.legend = NA,
                           inherit.aes = TRUE,
                           na.rm = FALSE,
                           type = "kaplan-meier",
                           table.type = "r",
                           firstx = 0, firsty = 1, n.breaks = 5, breaks = NULL, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatRiskTable,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      firstx = firstx,
      firsty = firsty,
      table.type = table.type,
      type = type,
      ...)
  )
}


#' @export

StatRiskTable <- ggplot2::ggproto("StatRiskTable", Stat,
                                  compute_group = function(self, data, scales, type = "kaplan-meier", firstx = 0, firsty = 1, table.type = table.type) {
                                    if(is.null(data$y)) {data$y <- 1}

                                    sf <- survfit.formula(Surv(data$x, data$status) ~ data$y, type = type, start.time = firstx)
                                    sf <- survfit0(sf, start.time = 0)
                                    sf_table <- cut_timepoints(sf, table.type = table.type) %>% rename(x = time)
                                    sf_table <- data.table(data)[data.table(sf_table), on = c("x"), roll = "nearest"]

                                    sf_table
                                  },


                                  required_aes = c("x", "status")
                                  )
