#' @import ggplot2
#' @import scales
#' @import grid
#' @import gtable
#' @import rlang
#' @importFrom glue glue glue_collapse

# Helpers ======================================================================

`%||%` <- function(a, b) {
  if(!is.null(a))
    a
  else b
}

`%|W|%` <- function (a, b)
{
  if (!is.waive(a))
    a
  else b
}

# Type Checks ==================================================================

is.discrete <- function(x) { is.factor(x) || is.character(x) || is.logical(x) }

is.zero <- function (x) { is.null(x) || inherits(x, "zeroGrob") }

is.waive <- function(x) {inherits(x, "waiver")}

is.ggtte <- function(x) {inherits(x, "ggtte")}

is.ggtte_layer <- function(x) {inherits(x, "ggtte_layer")}

is.ggtte_options <- function(x) {inherits(x, "ggtte_options")}

is.ggtte_scale <- function(x) {inherits(x, "ggtte_scale")}

is_theme_complete <- function(x) {
  isTRUE(attr(x, "complete", exact = TRUE))
}

is_theme_validate <- function(x) {
  validate <- attr(x, "validate", exact = TRUE)
  if (is.null(validate))
    TRUE
  else isTRUE(validate)
}
# Survival Functions ===========================================================

#' @export
cut_timepoints <- function(sf, table.type = "r") {
  sf_est <- broom::tidy(sf)
  labels <- switch(
    table.type,
    r = glue::glue("{sf_est$n.risk}"),
    re = glue::glue("{sf_est$n.risk} ({sf_est$n.event})"),
    rc = glue::glue("{sf_est$n.risk} ({sf_est$n.censor})"),
    rce = glue::glue("{sf_est$n.risk} ({sf_est$cum.event})"),
    rcc = glue::glue("{sf_est$n.risk} ({sf_est$cum.censor})")
  )
  sf_est$label <- labels
  sf_est
}

#' @importFrom data.table :=
tidy.survfit <- function (x, ...)
{
  strata <- time <- n.event <- n.censor <- NULL
    if (inherits(x, "survfitms")) {
      ret <- data.table::data.table(time = x$time, n.risk = c(x$n.risk),
                        n.event = c(x$n.event), n.censor = c(x$n.censor),
                        estimate = c(x$pstate), std.error = c(x$std.err),
                        conf.high = c(x$upper), conf.low = c(x$lower), state = rep(x$states,
                                                                                   each = nrow(x$pstate)))
      ret <- ret[ret$state != "", ]
    }
    else {
      ret <- data.table::data.table(time = x$time, n.risk = x$n.risk, n.event = x$n.event,
                        n.censor = x$n.censor, estimate = x$surv, std.error = x$std.err,
                        conf.high = x$upper, conf.low = x$lower)
    }
    if (!is.null(x$strata)) {
      ret$strata <- rep(names(x$strata), x$strata)
      data.table::setkey(ret, strata)
    }
  ret[order(time), `:=`(
    cum.event = cumsum(n.event),
    cum.censor = cumsum(n.censor)
  ), by = setdiff("", data.table::key(ret))]
  ret[]
}

# ggside =======================================================================

do_by <- function (data, by, fun, ...)
{
  order_cache <- do.call("order", lapply(by, function(x) {
    data[[x]]
  }))
  data <- data[order_cache, ]
  split_by <- interaction(data[, by, drop = F], drop = T, lex.order = T)
  data <- ggint$rbind_dfs(lapply(split(data, split_by), FUN = fun,
                                 ...))
  data <- data[order(order_cache), ]
  rownames(data) <- seq_len(nrow(data))
  data
}

wrapup <- function (df, by, ...)
{
  if (...length() == 0)
    return(df)
  indx <- interaction(df[, by], drop = T)
  indx <- match(indx, unique(indx))
  dots_ <- list(...)
  if (!all(unlist(lapply(dots_, function(x, y) {
    all(x %in% y)
  }, y = colnames(df)))))
    abort("all RHS must exist in column names of `df`.")
  wrap_columns <- unlist(dots_)
  l_ <- split(df, indx)
  l_ <- lapply(l_, function(x, d) {
    wrap <- lapply(d, function(y) list(x[, y, drop = FALSE]))
    x <- unique(x[, setdiff(colnames(x), wrap_columns), drop = FALSE])
    x[, names(d)] <- wrap
    x
  }, d = dots_)
  data <- ggint$rbind_dfs(l_)
  data
}

unwrap <- function (df, by, cols = NULL)
{
  if (is.null(cols))
    return(df)
  if (!all(cols %in% colnames(df)))
    abort("all `cols` must exist in column names of `df`")
  indx <- interaction(df[, by], drop = T)
  indx <- match(indx, unique(indx))
  l_ <- split(df, indx)
  l_ <- lapply(l_, function(x) {
    nest <- do.call("cbind", unlist(Map(function(d,
                                                 y) {
      d[, y, drop = T]
    }, d = list(x), y = cols), recursive = F))
    x <- x[, setdiff(colnames(x), cols), drop = FALSE]
    if (nrow(x) != 1)
      stop("by must uniquely index df")
    cbind(x[rep(1, nrow(nest)), ], nest)
  })
  data <- ggint$rbind_dfs(l_)
  data
}

# Class Checks =================================================================

#' @export
ggtte <- function(pos = "bottom", scales = "fixed", collapse = NULL) {
  structure(
    list(
      pos = pos,
      scales = scales,
      collapse = collapse,
      tte = NULL
    ), class = c("ggtte_options", "gg")
  )
}


