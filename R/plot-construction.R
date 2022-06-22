#'@export
validate_ggtte <- function(e2, object) { UseMethod('validate_ggtte') }

validate_ggtte.default <- function(e2, object) object

validate_ggtte.Facet <- function(e2, object) {
  object[['facet']] <- as_ggtteFacet(object[['facet']], object[['ggtte']])
  object
}

validate_ggtte.Coord <- function(e2, object) {
  object[['coordinates']] <- as_ggtteCoord(object[['coordinates']])
  object
}


#'@export
as_ggtte <- function(x, ...) { UseMethod('as_ggtte') }

as_ggtte.default <- function(x, ...) { abort(glue::glue('No as_ggtte() method for class <', glue_collapse(class(x), sep = '/'), '>')) }

as_ggtte.ggplot <- function(x, ggtte = NULL, ...) {
  if(inherits(x[['coordinates']], 'CoordFlip')||inherits(x[['coordinates']], 'CoordPolar')){
    abort('ggtte is not currently compatible with CoordFlip or CoordPolar')
  }
  ggtte <- ggtte %||% ggtte()
  if(!is.ggtte_options(ggtte)) stop('argument ggtte must be of class `ggtte_options` or NULL')
  class(x) <- c('ggtte', class(x))
  x[['ggtte']] <- ggtte
  update_ggtte(x)
}

as_ggtte.ggtte <- function(x, ggtte = NULL, ...){
  ggtte <- ggtte %||% x[['ggtte']] %||% ggtte()
  if(!is.ggtte_options(ggtte)) stop('argument ggtte must be of class `ggtte_options` or NULL')
  update_ggtte(x, ggtte)
}

as_ggtteCoord <- function(coord) {
  UseMethod("as_ggtteCoord")
}

guess_layer_mapping <- function(layer) {
  geom_class <- any(grepl('ggtte_layer', class(layer)))
  val <- if(geom_class) {'tte'} else { 'main' }
  return(val)
}

get_panels <- function(layers) {
  layer_mappings <- lapply(layers, guess_layer_mapping)
  panels_used <- unlist(layer_mappings)
  return(unique(panels_used))
}

#'@export
update_ggtte <- function(object, ggtte){ UseMethod('update_ggtte') }

update_ggtte.default <- function(object, ggtte) { abort(glue::glue('No update_ggtte() method for class <', glue_collapse(class(object), sep = '/'), '>')) }

update_ggtte.ggplot <- function(object, ggtte = NULL) {
  object$ggtte$pos <- ggtte$pos %||% object$ggtte$pos %||% 'bottom'
  if(!object$ggtte$pos %in% c('top', 'bottom')) {
    abort('pos may only by "top" or "bottom"')
  }
  object$ggtte$scales <- ggtte$scales %||% object$ggtte$scales %||% 'fixed'
  object$ggtte$panels_used <- get_panels(object[['layers']])
  object$ggtte$collapse <- ggtte$collapse %||% object$ggtte$collapse %||% NULL
  object$ggtte$tte <- ggtte$tte %||% object$ggtte$tte %||% NULL
  object[['facet']] <- as_ggtteFacet(object[['facet']], object[['ggtte']])
  object[['coordinates']] <- as_ggtteCoord(object[['coordinates']])
  return(object)
}

#'@importFrom ggplot2 ggplot_add
#'@export
ggplot_add.ggtte_layer <- function(object, plot, object_name) {
  p <- NextMethod('ggplot_add')
  as_ggtte(p)
}

ggplot_add.ggtte_options <- function(object, plot, object_name) {
  as_ggtte(plot, object)
}

ggplot_add.ggtte_scale <- function(object, plot, object_name) {
  plot$ggtte[[intersect(c('tte'), object$aesthetics)]] <- object
  as_ggtte(plot)
}
