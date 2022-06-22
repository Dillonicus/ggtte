tteFacetNull_draw_panels <- function(panels, layout, x_scales, y_scales,
                                      ranges, coord, data, theme, params) {

  params <- params$ggtte
  ncol <- max(layout$COL)
  nrow <- max(layout$ROW)
  n <- nrow(layout)
  panel_order <- order(layout$ROW, layout$COL)
  layout <- layout[panel_order,]
  panels <- panels[panel_order]
  panel_pos <- ggint$convertInd(layout$ROW, layout$COL, nrow)
  layout$panel_pos <- panel_pos
  tte_panels_present <- c("tte")[c("tte") %in% layout$PANEL_TYPE]
  pos <- params$pos
  panels_used <- params$panels_used

  aspect_ratio <- theme$aspect.ratio
  if(is.null(aspect_ratio)) {
    aspect_ratio <- coord$aspect(ranges[[1]])
  }

  if(is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else { respect <- TRUE }

  tte.panel.scale <- theme$ggtte.panel.scale %||% 0.3

  empty_table <- matrix(list(ggplot2::zeroGrob()), nrow = nrow, ncol = ncol)
  panel_table <- empty_table
  panel_table[panel_pos] <- panels
  empties <- apply(panel_table, c(1, 2), function(x) is.zero(x[[1]]))

  p.widths <- unit(rep(1, ncol), "null")

  p.heights <- if("tte" %in% panels_used && "main" %in% panels_used) {
    .heights <- c(aspect_ratio, aspect_ratio * tte.panel.scale)
    .tmp <- layout[layout[["PANEL_TYPE"]] == "tte", ][["ROW"]]
    if(pos == "top") {
      .heights <- rev(.heights)
    }
    unit(rep(abs(.heights), nrow/2), "null")
  } else {
    unit(rep(aspect_ratio, nrow), "null")
  }

  panel_table <- gtable::gtable_matrix(
    "layout",
    panel_table,
    widths = p.widths,
    heights = p.heights,
    respect = respect,
    clip = coord$clip,
    z = matrix(1, ncol = ncol, nrow = nrow)
  )

  panel_table$layout$name <- paste0("panel-", rep(seq_len(ncol), nrow), "-", rep(seq_len(nrow), each = ncol))
  axes <- render_axes(ranges[1], ranges, coord, theme, transpose = TRUE)

  # Register theme elements
  mainpanel_spacing <- theme$panel.spacing.y %||% theme$panel.spacing
  ttepanel_spacing <- theme$ggtte.panel.spacing %||% theme$panel.spacing

  row.heights <- if("tte" %in% panels_used && "main" %in% panels_used) {
    unit(rep(c(ttepanel_spacing, mainpanel_spacing), length.out = length(panel_table$heights) - 1), unitType(ttepanel_spacing))
  } else {
    mainpanel_spacing
  }

  panel_table <- gtable::gtable_add_row_space(panel_table, row.heights)

  axis_mat_x_top <- empty_table
  axis_mat_x_top[panel_pos] <- axes$x$top
  axis_mat_x_bottom <- empty_table
  axis_mat_x_bottom[panel_pos] <- axes$x$bottom
  axis_mat_y_left <- empty_table
  axis_mat_y_left[panel_pos] <- axes$y$left
  axis_mat_y_right <- empty_table
  axis_mat_y_right[panel_pos] <- axes$y$right

  .xgroupby <- "COL"
  .ygroupby <- "ROW"

  bottom <- do_by(layout, "COL", function(x){x[["ROW2"]] <- max(x[["ROW"]]); x})
  top <- do_by(layout, "COL", function(x){x[["ROW2"]] <- min(x[["ROW"]]); x})
  right <- do_by(layout, "ROW", function(x){x[["COL2"]] <- max(x[["COL"]]); x})
  left <- do_by(layout, "ROW", function(x){x[["COL2"]] <- min(x[["COL"]]); x})

  if(params$scales%in%c("free","free_y")){ #if y is free, include x PANELS_TYPES
    right <- right[right[["COL"]]==right[["COL2"]]|right[["PANEL_TYPE"]]=="x",]
    left <- left[left[["COL"]]==left[["COL2"]]|left[["PANEL_TYPE"]]=="x",]
  } else {
    right <- right[right[["COL"]]==right[["COL2"]],]
    left <- left[left[["COL"]]==left[["COL2"]],]
  }
  if(params$scales%in%c("free","free_x")){ #if x is free, include y PANELS_TYPES
    top <- top[top[["ROW"]]==top[["ROW2"]]|top[["PANEL_TYPE"]]=="y",]
    bottom <- bottom[bottom[["ROW"]]==bottom[["ROW2"]]|bottom[["PANEL_TYPE"]]=="y",]
  } else {
    top <- top[top[["ROW"]]==top[["ROW2"]],]
    bottom <- bottom[bottom[["ROW"]]==bottom[["ROW2"]],]
  }

  bottom <- dplyr::anti_join(layout, bottom, by = c("ROW","COL"))[["panel_pos"]]
  top <- dplyr::anti_join(layout, top, by = c("ROW","COL"))[["panel_pos"]]
  right <- dplyr::anti_join(layout, right, by = c("ROW","COL"))[["panel_pos"]]
  left <- dplyr::anti_join(layout, left, by = c("ROW","COL"))[["panel_pos"]]

  #pulled panel positions
  #Place ZeroGrobs
  axis_mat_x_top[top]<- list(ggplot2::zeroGrob())
  axis_mat_x_bottom[bottom]<- list(ggplot2::zeroGrob())
  axis_mat_y_left[left] <- list(ggplot2::zeroGrob())
  axis_mat_y_right[right] <- list(ggplot2::zeroGrob())

  axis_height_top <- unit(
    apply(axis_mat_x_top, 1, ggplot2::max_height, value_only = TRUE),
    "cm"
  )
  axis_height_bottom <- unit(
    apply(axis_mat_x_bottom, 1, ggplot2::max_height, value_only = TRUE),
    "cm"
  )
  axis_width_left <- unit(
    apply(axis_mat_y_left, 2, ggplot2::max_width, value_only = TRUE),
    "cm"
  )
  axis_width_right <- unit(
    apply(axis_mat_y_right, 2, ggplot2::max_width, value_only = TRUE),
    "cm"
  )
  panel_table <- ggint$weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
  panel_table <- ggint$weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
  panel_table <- ggint$weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
  panel_table <- ggint$weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)
  panel_table
}


map_data_tte <- function(data, layout, params){

  if (is.waive(data))
    return(new_data_frame(list(PANEL = factor())))

  if (ggplot2:::empty(data))
    return(new_data_frame(c(data, list(PANEL = factor()))))

  facet_vars <- c(names(params$facets),names(params$rows),names(params$cols))

  if(!"PANEL_TYPE"%in%colnames(data)){
    data$PANEL_TYPE <- "main"
  }
  layout <- unwrap(layout, c("ROW","COL"), "FACET_VARS")
  keys <- ggplot2:::join_keys(data, layout, by = c("PANEL_TYPE",facet_vars))
  data[["PANEL"]] <- layout[["PANEL"]][match(keys$x, keys$y)]

  data
}

ttePanelLayout <- function(layout, params) {
  facet_vars <- setdiff(colnames(layout), c("PANEL","ROW","COL","SCALE_X","SCALE_Y","PANEL_GROUP","PANEL_TYPE"))
  pos <- params$pos
  scales <- params$scales
  collapse <- params$collapse %||% "default"
  ttePanel <- params$panels_used
  surv_both <- all(c("tte", "main") %in% ttePanel)
  if(surv_both) {
    xrow <- ifelse(pos == "top", "ODD", "EVEN")
    mrow <- ifelse(xrow == "EVEN", "ODD", "EVEN")
  }

  else {
    xrow <- "ALL"
    mrow <- "ALL"
  }

  data <- data.frame(
    PANEL_TYPE = c("main", "tte"),
    ROW_trans = c(mrow, xrow)
  )

  data <- data[data$PANEL_TYPE %in% c(ttePanel),]
  free_fun <- function(x, lgl){
    max(x)+(seq_len(sum(lgl)))
  }

  fixed_fun <- function(x, lgl){
    rep(max(x)+1L,sum(lgl))
  }

  scale_fun <- switch(
    scales,
    free_y = free_fun,
    free = free_fun, fixed_fun)

  layout$PANEL_GROUP <- layout$PANEL
  layout <- cbind.data.frame(
    layout[rep(1:nrow(layout), each = nrow(data)), ],
    data[rep(1:nrow(data), nrow(layout)),]
  )

  layout[["GROUP_ROW"]] <- layout[["ROW"]]
  layout[["ROW"]] <- layout[["ROW"]] * ifelse(layout[["ROW_trans"]] == "ALL", 1L, 2L) - ifelse(layout[["ROW_trans"]]=="ODD", 1L, 0L)

  if(surv_both) {
    .ptsurv <- layout[["PANEL_TYPE"]] == "tte"
    layout[["SCALE_Y"]][.ptsurv] <- scale_fun(layout[["SCALE_Y"]], .ptsurv)
  }

  layout <- layout[, setdiff(colnames(layout), c("ROW_trans", "COL_trans", "PANEL"))]
  layout <- unique(layout)
  layout <- layout[order(layout$ROW, layout$COL),]
  layout <- wrapup(layout, by = c("ROW", "COL"), FACET_VARS = facet_vars)
  layout$PANEL <- factor(1:nrow(layout))
  return(layout)
}

tte_train_scales = function(x_scales, y_scales, layout, data, params) {
  for (layer_data in data) {
    match_id <- match(layer_data$PANEL, layout$PANEL)
    if (!is.null(x_scales)) {
      x_vars <- intersect(x_scales[[1]]$aesthetics, names(layer_data))
      SCALE_X <- layout$SCALE_X[match_id]
      #lapply(x_scales, function(x) x$reset())
      ggint$scale_apply(layer_data, x_vars, "train", SCALE_X,
                        x_scales)
    }
    if (!is.null(y_scales)) {
      y_vars <- intersect(y_scales[[1]]$aesthetics, names(layer_data))
      SCALE_Y <- layout$SCALE_Y[match_id]
      #lapply(y_scales, function(y) if('ggtte_scale' %in% class(y)) y$reset())
      ggint$scale_apply(layer_data, y_vars, "train", SCALE_Y,
                        y_scales)
    }
  }
}

FacetRiskNull <- ggplot2::ggproto("FacetRiskNull",
                                  FacetNull,
                                  compute_layout = function(data, params){
                                    layout <- ggplot2::FacetNull$compute_layout(data, params)
                                    layout <- ttePanelLayout(layout = layout, params = params$ggtte)
                                    layout
                                    },
                                  init_scales = function(layout, x_scale = NULL, y_scale = NULL, params){
                                    scales <- FacetNull$init_scales(layout, x_scale, y_scale, params)
                                    if(!is.null(y_scale)&& !is.null(params$ggtte$tte)) {
                                      side_indx <- layout[layout$PANEL_TYPE == 'tte',]$SCALE_Y
                                      scales$y[side_indx] <- lapply(side_indx, function(i) params$ggtte$tte$clone())
                                    }
                                    scales
                                  },
                                  train_scales = tte_train_scales,
                                  map_data = map_data_tte,
                                  draw_panels = tteFacetNull_draw_panels,
                                  draw_labels = function (panels, layout, x_scales, y_scales, ranges, coord,
                                                          data, theme, labels, params)
                                  {
                                    panel_dim <- find_panel(panels)
                                    xlab_height_top <- grobHeight(labels$x[[1]])
                                    panels <- gtable_add_rows(panels, xlab_height_top, pos = 0)
                                    panels <- gtable_add_grob(panels, labels$x[[1]], name = "xlab-t",
                                                              l = panel_dim$l, r = panel_dim$r, t = 1, clip = "off")
                                    xlab_height_bottom <- grobHeight(labels$x[[2]])
                                    panels <- gtable_add_rows(panels, xlab_height_bottom, pos = -1)
                                    panels <- gtable_add_grob(panels, labels$x[[2]], name = "xlab-b",
                                                              l = panel_dim$l, r = panel_dim$r, t = -1, clip = "off")
                                    panel_dim <- find_panel(panels)
                                    ylab_width_left <- grobWidth(labels$y[[1]])
                                    panels <- gtable_add_cols(panels, ylab_width_left, pos = 0)
                                    panels <- gtable_add_grob(panels, labels$y[[1]], name = "ylab-l-1",
                                                              l = 1, b = panel_dim$t, t = panel_dim$t, clip = "off")
                                    panels <- gtable_add_grob(panels, labels$y[[3]], name = "ylab-l-2",
                                                              l = 1, b = panel_dim$b, t = panel_dim$b, clip = "off")
                                    ylab_width_right <- grobWidth(labels$y[[2]])
                                    panels <- gtable_add_cols(panels, ylab_width_right, pos = -1)
                                    panels <- gtable_add_grob(panels, labels$y[[2]], name = "ylab-r",
                                                              l = -1, b = panel_dim$b, t = panel_dim$t, clip = "off")
                                    panels
                                  })
