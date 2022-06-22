facet_riskwrap <- function(shrink = TRUE, position = "bottom", scales = "fixed") {
  ggproto(NULL, FacetRiskWrap, shrink = shrink, params = list(pos = position, scales = scales))
}

FacetRiskWrap <- ggproto(
  "FacetRiskWrap", FacetWrap,

  compute_layout = function(data, params) {
    layout <- FacetWrap$compute_layout(data, params)
    layout <- ttePanelLayout(layout = layout, params = params$ggtte)
    layout
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

    if(!"PANEL_TYPE" %in% colnames(data)){
      data$PANEL_TYPE <- "main"
    }

    layout <- unwrap(layout, c("ROW","COL"), "FACET_VARS")

    keys <- ggint$join_keys(data, layout, by = c("PANEL_TYPE",facet_vars))
    data[["PANEL"]] <- layout[["PANEL"]][match(keys$x, keys$y)]
    data
  },
  draw_panels = function(panels, layout, x_scales, y_scales,
                         ranges, coord, data, theme, params) {

    #params <- params$ggtte

    ncol <- max(layout$COL)
    nrow <- max(layout$ROW)
    nsubrow <- length(unique(layout$PANEL_TYPE))
    n <- nrow(layout)
    panel_order <- order(layout$ROW, layout$COL)
    layout <- layout[panel_order,]
    panels <- panels[panel_order]
    panel_pos <- ggint$convertInd(layout$ROW, layout$COL, nrow)
    layout$panel_pos <- panel_pos
    tte_panels_present <- c("tte")[c("tte") %in% layout$PANEL_TYPE]
    pos <- params$ggtte$pos
    panels_used <- params$ggtte$panels_used

    axes <- ggplot2::render_axes(ranges, ranges, coord, theme, transpose = TRUE)
    layout <- unwrap(layout, by = c("ROW","COL"),cols = "FACET_VARS")
    if(length(params$facets) == 0) {
      labels_df <- ggint$new_data_frame(list("(all)" = "(all)"), n = 1)
    } else {
      labels_df <- unique(layout[, names(params$facets), drop = FALSE])
      #labels_df <- unique(layout[layout[["PANEL_TYPE"]] == "main", names(params$facets), drop = FALSE])
    }

    attr(labels_df, "facet") <- "wrap"
    strips <- ggplot2::render_strips(
      structure(labels_df, type = "rows"),
      structure(labels_df, type = "cols"),
      params$labeller, theme
    )

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
      "layout", panel_table,
      widths = p.widths,
      heights = p.heights, respect = respect,
      clip = coord$clip, z = matrix(1, ncol = ncol, nrow = nrow)
    )

    #panel_table$layout$name <- paste0("panel-", rep(seq_len(ncol), nrow), "-", rep(seq_len(nrow), each = ncol))
    panel_table$layout$name <- paste0("panel-", panel_table$layout$t, "-", panel_table$layout$l)
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

    strip_padding <- grid::convertUnit(theme$strip.switch.pad.wrap, "cm")
    strip_name <- paste0("strip-", substr(params$strip.position, 1, 1))
    strip_mat <- empty_table
    strip_mat[layout$panel_pos[order(layout$PANEL_GROUP)]] <- rep(unlist(unname(strips), recursive = FALSE)[[params$strip.position]], each = nsubrow)

    if(all(c("main", "tte") %in% panels_used)) {
      strip_mat <- strip_mat_clean(strip_mat, layout, params$strip.position)
    }
    if (params$strip.position %in% c("top", "bottom")) {
      inside_x <- (theme$strip.placement.x %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "top") {
        placement <- if (inside_x) -1 else -2
        strip_pad <- axis_height_top
      } else {
        placement <- if (inside_x) 0 else 1
        strip_pad <- axis_height_bottom
      }
      strip_height <- unit(apply(strip_mat, 1, ggplot2::max_height, value_only = TRUE), "cm")
      panel_table <- ggint$weave_tables_row(panel_table, strip_mat, placement, strip_height, strip_name, 2, coord$clip)
      if (!inside_x) {
        strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
        panel_table <- ggint$weave_tables_row(panel_table, row_shift = placement, row_height = strip_pad)
      }
    } else {
      inside_y <- (theme$strip.placement.y %||% theme$strip.placement %||% "inside") == "inside"
      if (params$strip.position == "left") {
        placement <- if (inside_y) -1 else -2
        strip_pad <- axis_width_left
      } else {
        placement <- if (inside_y) 0 else 1
        strip_pad <- axis_width_right
      }
      strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
      strip_width <- unit(apply(strip_mat, 2, ggplot2::max_width, value_only = TRUE), "cm")
      panel_table <- ggint$weave_tables_col(panel_table, strip_mat, placement, strip_width, strip_name, 2, coord$clip)
      if (!inside_y) {
        strip_pad[as.numeric(strip_pad) != 0] <- strip_padding
        panel_table <- ggint$weave_tables_col(panel_table, col_shift = placement, col_width = strip_pad)
      }
    }
    panel_table
  },
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

strip_mat_clean <- function(strip_mat, layout, side) {
  strip_pos <- switch(side, top = 1, bottom = 2, left = NA, right = NA)
  zero_ind <- tapply(layout$panel_pos, layout$PANEL_GROUP, function(x) x[-strip_pos])
  strip_mat[zero_ind] <- list(ggplot2::zeroGrob())
  strip_mat
}

panel_dims <- function(table, layout) {
  ROW <- PANEL_TYPE <- l <- b <- r <- t <- NULL
  panels <- table$layout
  panels <- panels[grepl('^panel', panels$name), , drop = TRUE]
  panels
  panels$ROW <- as.numeric(gsub('panel-(.*)-\\d+', '\\1', panels$name))
  panels$COL <- as.numeric(gsub('panel-\\d+-(.*)$', '\\1', panels$name))
  layout %>%
    dplyr::left_join(panels, by = c("ROW", "COL")) %>%
    dplyr::group_by(ROW, PANEL_TYPE) %>%
    dplyr::summarize(t = min(t), l = min(l), b = max(b), r = max(r), .groups = "drop") %>%
    dplyr::select(-ROW) %>%
    unique()
}
