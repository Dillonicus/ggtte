tteFacetGrid_draw_panels <- function(panels, layout, x_scales, y_scales,
                                      ranges, coord, data, theme, params) {
  ncol <- max(layout$COL)
  nrow <- max(layout$ROW)
  n <- nrow(layout)
  panel_order <- order(layout$ROW, layout$COL)
  layout <- layout[panel_order,]
  panels <- panels[panel_order]
  panel_pos <- ggint$convertInd(layout$ROW, layout$COL, nrow)
  layout$panel_pos <- panel_pos
  tte_panels_present <- c("tte")[c("tte") %in% layout$PANEL_TYPE]
  pos <- params$ggtte$pos
  panels_used <- params$ggtte$panels_used
  layout <- unwrap(layout, by = c("ROW","COL"),cols = "FACET_VARS")

  col_vars <- unique(layout[names(params$cols)])
  row_vars <- unique(layout[names(params$rows)])

  attr(col_vars, "type")  <- "cols"
  attr(col_vars, "facet") <- "grid"
  attr(row_vars, "type")  <- "rows"
  attr(row_vars, "facet") <- "grid"
  strips <- ggplot2::render_strips(col_vars, row_vars, params$labeller, theme)

  aspect_ratio <- theme$aspect.ratio
  if(is.null(aspect_ratio)) {
    aspect_ratio <- coord$aspect(ranges[[1]])
  }

  if(is.null(aspect_ratio)) {
    aspect_ratio <- 1
    respect <- FALSE
  } else { respect <- TRUE }

  tte.panel.scale <- theme$ggtte.panel.scale %||% 0.3

  empty_table <- matrix(list(zeroGrob()), nrow = nrow, ncol = ncol)
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

  #panel_table$layout$name <- paste0("panel-", rep(seq_len(ncol), nrow), "-", rep(seq_len(nrow), each = ncol), "-group-", layout$PANEL_GROUP[order(layout$panel_pos)])
  panel_table$layout$name <- paste0("panel-", panel_table$layout$t, "-", panel_table$layout$l)

  axes <- ggplot2::render_axes(ranges, ranges, coord, theme, transpose = TRUE)


  # # Register theme elements
  # mainpanel_spacing <- theme$panel.spacing.y %||% theme$panel.spacing
  # ttepanel_spacing <- theme$ggtte.panel.spacing %||% theme$panel.spacing
  #
  #
  # col.widths <- xpanel_spacing
  #
  # row.heights <- if("tte" %in% panels_used && "main" %in% panels_used) {
  #   unit(rep(c(mainpanel_spacing, ttepanel_spacing), length.out = length(panel_table$heights) - 1), "pt")
  # } else {
  #   mainpanel_spacing
  # }
  # Register theme elements

  xpanel_spacing <- theme$panel.spacing.x %||% theme$panel.spacing
  ypanel_spacing <- theme$panel.spacing.y %||% theme$panel.spacing
  ttepanel_spacing_x <- theme$ggtte.panel.spacing.x %||% theme$ggtte.panel.spacing %||% theme$panel.spacing
  ttepanel_spacing_y <- theme$ggtte.panel.spacing.y %||% theme$ggtte.panel.spacing %||% theme$panel.spacing

  row.heights <- if(all(c("tte", "main") %in% panels_used)) {
    unit(rep(c(ttepanel_spacing_y, ypanel_spacing), length.out = length(panel_table$heights) - 1), unitType(ttepanel_spacing_y))
  } else {
    ypanel_spacing
  }

  col.widths <- xpanel_spacing
  panel_table <- gtable::gtable_add_col_space(panel_table, col.widths)
  panel_table <- gtable::gtable_add_row_space(panel_table, row.heights)

  axis_mat_x_top <- empty_table
  axis_mat_x_top[panel_pos] <- axes$x$top
  axis_mat_x_bottom <- empty_table
  axis_mat_x_bottom[panel_pos] <- axes$x$bottom
  axis_mat_y_left <- empty_table
  axis_mat_y_left[panel_pos] <- axes$y$left
  axis_mat_y_right <- empty_table
  axis_mat_y_right[panel_pos] <- axes$y$right

  if (!params$free$x) {
    axis_mat_x_top[-1,]<- list(ggplot2::zeroGrob())
    axis_mat_x_bottom[-c(nrow),]<- list(ggplot2::zeroGrob())
  }
  if (!params$free$y) {
    axis_mat_y_left[, -1] <- list(ggplot2::zeroGrob())
    axis_mat_y_right[, -ncol] <- list(ggplot2::zeroGrob())
  }

  bottom <- do_by(layout, "COL", function(x){x[["ROW2"]] <- max(x[["ROW"]]); x})
  top <- do_by(layout, "COL", function(x){x[["ROW2"]] <- min(x[["ROW"]]); x})
  right <- do_by(layout, "ROW", function(x){x[["COL2"]] <- max(x[["COL"]]); x})
  left <- do_by(layout, "ROW", function(x){x[["COL2"]] <- min(x[["COL"]]); x})

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

  axis_height_top <- grid::unit(
    apply(axis_mat_x_top, 1, ggplot2::max_height, value_only = TRUE),
    "cm"
  )
  axis_height_bottom <- grid::unit(
    apply(axis_mat_x_bottom, 1, ggplot2::max_height, value_only = TRUE),
    "cm"
  )
  axis_width_left <- grid::unit(
    apply(axis_mat_y_left, 2, ggplot2::max_width, value_only = TRUE),
    "cm"
  )
  axis_width_right <- grid::unit(
    apply(axis_mat_y_right, 2, ggplot2::max_width, value_only = TRUE),
    "cm"
  )

  panel_table <- ggint$weave_tables_row(panel_table, axis_mat_x_top, -1, axis_height_top, "axis-t", 3)
  panel_table <- ggint$weave_tables_row(panel_table, axis_mat_x_bottom, 0, axis_height_bottom, "axis-b", 3)
  panel_table <- ggint$weave_tables_col(panel_table, axis_mat_y_left, -1, axis_width_left, "axis-l", 3)
  panel_table <- ggint$weave_tables_col(panel_table, axis_mat_y_right, 0, axis_width_right, "axis-r", 3)

  switch_x <- !is.null(params$switch) && params$switch %in% c("both", "x")
  switch_y <- !is.null(params$switch) && params$switch %in% c("both", "y")
  inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
  inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"

  strip_padding <- grid::convertUnit(theme$strip.switch.pad.wrap, "cm")
  panel_pos_col <- panel_cols(panel_table)
  if (switch_x) {
    if (!is.null(strips$x$bottom)) {
      if (inside_x) {
        panel_table <- gtable::gtable_add_rows(panel_table, max_height(strips$x$bottom), -2)
        panel_table <- gtable::gtable_add_grob(panel_table, strips$x$bottom, -2, panel_pos_col$l, clip = "on", name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
      } else {
        panel_table <- gtable::gtable_add_rows(panel_table, strip_padding, -1)
        panel_table <- gtable::gtable_add_rows(panel_table, ggplot2::max_height(strips$x$bottom), -1)
        panel_table <- gtable::gtable_add_grob(panel_table, strips$x$bottom, -1, panel_pos_col$l, clip = "on", name = paste0("strip-b-", seq_along(strips$x$bottom)), z = 2)
      }
    }
  } else {
    if (!is.null(strips$x$top)) {
      if (inside_x) {
        panel_table <- gtable::gtable_add_rows(panel_table, ggplot2::max_height(strips$x$top), 1)
        panel_table <- gtable::gtable_add_grob(panel_table, strips$x$top, 2, panel_pos_col$l, clip = "on", name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
      } else {
        panel_table <- gtable::gtable_add_rows(panel_table, strip_padding, 0)
        panel_table <- gtable::gtable_add_rows(panel_table, ggplot2::max_height(strips$x$top), 0)
        panel_table <- gtable::gtable_add_grob(panel_table, strips$x$top, 1, panel_pos_col$l, clip = "on", name = paste0("strip-t-", seq_along(strips$x$top)), z = 2)
      }
    }
  }

  #panel_pos_rows <- panel_rows(panel_table)
  panel_pos_rows <- panel_rows_grouped(panel_table, layout)

  if (switch_y) {
    if (!is.null(strips$y$left)) {
      if (inside_y) {
        panel_table <- gtable::gtable_add_cols(panel_table, ggplot2::max_width(strips$y$left), 1)
        panel_table <- gtable::gtable_add_grob(panel_table, strips$y$left, panel_pos_rows$t, 2, clip = "on", name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
      } else {
        panel_table <- gtable::gtable_add_cols(panel_table, strip_padding, 0)
        panel_table <- gtable::gtable_add_cols(panel_table, ggplot2::max_width(strips$y$left), 0)
        panel_table <- gtable::gtable_add_grob(panel_table, strips$y$left, panel_pos_rows$t, 1, clip = "on", name = paste0("strip-l-", seq_along(strips$y$left)), z = 2)
      }
    }
  } else {
    if (!is.null(strips$y$right)) {
      if (inside_y) {
        panel_table <- gtable::gtable_add_cols(panel_table, ggplot2::max_width(strips$y$right), -2)
        panel_table <- gtable::gtable_add_grob(panel_table, strips$y$right, t = unique(panel_pos_rows$t), b = unique(panel_pos_rows$b), -2, clip = "on", name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)

      } else {
        panel_table <- gtable::gtable_add_cols(panel_table, strip_padding, -1)
        panel_table <- gtable::gtable_add_cols(panel_table, ggplot2::max_width(strips$y$right), -1)
        panel_table <- gtable::gtable_add_grob(panel_table, strips$y$right, t = unique(panel_pos_rows$t), b = unique(panel_pos_rows$b), -1, clip = "on", name = paste0("strip-r-", seq_along(strips$y$right)), z = 2)
      }
    }
  }
  panel_table
}

panel_rows_grouped <- function (table, layout) {
  l <- b <- r <- t <- NULL
  panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
  panels$ROW <- as.numeric(gsub('panel-(.*)-\\d+', '\\1', panels$name))
  panels$COL <- as.numeric(gsub('panel-\\d+-(.*)$', '\\1', panels$name))
  panels %>%
    dplyr::left_join(layout, by = c("ROW", "COL")) %>%
    dplyr::group_by(GROUP_ROW) %>%
    dplyr::summarize(t = min(t), b = max(b), l = min(l), r = max(r), .groups = "drop") %>%
    unique()
}

panel_cols_grouped <- function (table) {
  panels <- table$layout[grepl("^panel", table$layout$name), , drop = FALSE]
  panels$group <- stringi::stri_extract(panels$name, regex = "group-\\d{1,}$")
  panels %>%
    dplyr::select(group, l, r) %>%
    tidyr::pivot_longer(cols = c("l", "r")) %>%
    dplyr::group_by(group) %>%
    dplyr::summarize(data = .data, l = min(value), r = max(value)) %>%
    dplyr::select(l, r) %>% unique()
}

FacetRiskGrid <- ggplot2::ggproto(
  "FacetRiskGrid",
  ggplot2::FacetGrid,
  compute_layout = function(data, params) {
    layout <- ggplot2::FacetGrid$compute_layout(data, params)
    layout <- ttePanelLayout(layout = layout, params = params$ggtte)
    return(layout)
  },
  init_scales = function(self, layout, x_scale = NULL, y_scale = NULL, params) {
    scales <- FacetNull$init_scales(layout, x_scale, y_scale, params)
    if(!is.null(y_scale)&& !is.null(params$ggtte$tte)) {
      side_indx <- layout[layout$PANEL_TYPE == 'tte',]$SCALE_Y
      scales$y[side_indx] <- lapply(side_indx, function(i) params$ggtte$tte$clone())
    }
    scales
  },
  map_data = function(data, layout, params){
    if (is.waive(data))
      data.frame(list(PANEL = factor()))

    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0)
      return(data.frame(c(data, list(PANEL = factor()))))

    facet_vars <- c(names(params$facets),names(params$rows),names(params$cols))

    if(!"PANEL_TYPE" %in% colnames(data)){data$PANEL_TYPE <- "main"}

    layout <- unwrap(layout, c("ROW","COL"), "FACET_VARS")
    keys <- ggint$join_keys(data, layout, by = c("PANEL_TYPE",facet_vars))
    data[["PANEL"]] <- layout[["PANEL"]][match(keys$x, keys$y)]
    data
  },
  draw_panels = tteFacetGrid_draw_panels,
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
    panel_dim <- panel_dims(panels, layout)
    panel_dim
    ylab_width_left <- grobWidth(labels$y[[1]])
    panels <- gtable_add_cols(panels, ylab_width_left, pos = 0)

    for (i in seq_len(nrow(panel_dim))) {
      pd <- panel_dim[i,]
      lab <- ifelse(pd[["PANEL_TYPE"]] == "main", 1, 3)
      panels <- gtable_add_grob(panels, labels$y[[lab]], name = paste0('ylab-l-', i),
                                l = 1, b = pd[["b"]], t = pd[['t']], clip = "off")
    }
    ylab_width_right <- grobWidth(labels$y[[2]])
    panels <- gtable_add_cols(panels, ylab_width_right, pos = -1)
    for (i in seq_len(nrow(panel_dim))) {
      pd <- panel_dim[i,]
      lab <- ifelse(pd[["PANEL_TYPE"]] == "main", 2, 4)
      panels <- gtable_add_grob(panels, labels$y[[lab]], name = paste0('ylab-r-', i),
                                l = -1, b = pd[["b"]], t = pd[['t']], clip = "off")
    }
    panels
  })

