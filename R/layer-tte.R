
find_parent <- function(find){
  for(N in seq_len(sys.parent())){
    if(find %in% ls(parent.frame(N))){
      obj <- get(find, envir = parent.frame(N))
      return(obj)
    }
  }
}

get_proper_scales <- function(data, scales){
  new_scale_list <- scales$clone()
  use_panels <- unique(data[['PANEL']])
  proto_layout <- find_parent('layout')
  layout_df <- proto_layout$layout


  layout_df <- layout_df[layout_df$PANEL %in% use_panels,]

  panel_type <- as.character(unique(layout_df[["PANEL_TYPE"]]))
  aesthetic <-switch(panel_type,

                     tte = 'y',
                     NULL)
  scale_ref <- switch(aesthetic,
                      y = proto_layout$panel_scales_y[layout_df$SCALE_Y][[1]],
                      NULL)
  if(is.null(scale_ref)) return(NULL)
  new_scale_list$scales[new_scale_list$find(aesthetic)] <- NULL
  new_scale_list$add(scale_ref$clone())
  return(new_scale_list)
}

TTELayer <- ggplot2::ggproto(
  "TTELayer",
  ggint$Layer,
  setup_layer = function(self, data, plot) {
    data <- ggproto_parent(ggint$Layer, self)$setup_layer(data = data, plot = plot)
    data$PANEL_TYPE <- "tte"
    data
  }
)

