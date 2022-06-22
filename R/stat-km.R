
#' @export

StatKM <- ggplot2::ggproto("StatKM", Stat,
                           compute_group = function(data, scales, type = 'kaplan-meier', trans = 'identity', firstx = 0, firsty = 1) {
                             sf <- survfit.formula(Surv(data$x, data$status) ~ 1, start.time = firstx)
                             sf <- survfit0(sf, start.time = 0)
                             sf <- broom::tidy(sf) %>% rename(x = time, y = estimate)
                             transloc <- scales::as.trans(trans)$trans
                             if(is_empty(sf)) {
                               x <- rep(sf$time, 2)
                               sf$surv <- rep(1, length(x))
                             }
                             step <- ggint$stairstep(sf, direction = 'hv') %>% rename(estimate = y)
                             step
                           },
                           required_aes = c('x', 'status'),
                           default_aes = aes(y = after_stat(estimate))
                           )

#' @export
stat_km <- function(mapping = NULL, data = NULL, geom = "KM",
                    position = "identity", show.legend = NA, inherit.aes = TRUE,
                    se = TRUE, trans = "identity", firstx = 0, firsty = 1,
                    type = "kaplan-meier") {
  ggplot2::layer(
    stat = StatKM,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trans = trans, firstx = firstx, firsty = firsty,
                  type = type)
  )

}
