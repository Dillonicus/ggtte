#'@export
as_ggtteFacet <- function(facet, ggtte) UseMethod("as_ggtteFacet")

as_ggtteFacet.default <- function(facet, ggtte){
  rlang::abort(glue::glue("No known method to make {class(facet)[1]} ggtte friendly"))
}

as_ggtteFacet.FacetNull <- function(facet, ggtte){
  params <- facet$params
  params[["ggtte"]] <- ggtte
  ggplot2::ggproto(NULL,
                   FacetRiskNull,
                   params = params,
                   shrink = facet$shrink)
}

as_ggtteFacet.FacetWrap <- function(facet, ggtte){
  params <- facet$params
  params[['ggtte']] <- ggtte
  ggplot2::ggproto(NULL,
                   FacetRiskWrap,
                   params = params,
                   shrink = facet$shrink)
}


as_ggtteFacet.FacetGrid <- function(facet, ggtte){
  params <- facet$params
  params[['ggtte']] <- ggtte
  ggplot2::ggproto(NULL,
                   FacetRiskGrid,
                   params = params,
                   shrink = facet$shrink)
}
