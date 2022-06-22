#'
.onLoad <- function(libname, pkgname) {
  ggplot2::register_theme_elements(
    axis.title.y.left.risk = NULL,
    axis.title.y.right.risk = NULL,
    element_tree = list(
      axis.title.y.right.risk = el_def("element_text", "axis.title.y"),
      axis.title.y.left.risk = el_def("element_text", "axis.title.y")
      )
    )
}
