.internals <- unique(
  c(
    'add_group',
    'as_gg_data_frame',
    'check_aesthetics',
    'check_nondata_cols',
    'check_required_aesthetics',
    'continuous_range',
    'convertInd',
    'dapply',
    'defaults',
    'discrete_range',
    'empty',
    'get_alt_text',
    'is_calculated_aes',
    'is_scaled_aes',
    'is_theme_complete',
    'is_theme_validate',
    'join_keys',
    'Layer',
    'Layout',
    'make_labels',
    'new_data_frame',
    'new_mapped_discrete',
    'plot_clone',
    'rbind_dfs',
    'scale_apply',
    'scales_add_defaults',
    'scales_add_missing',
    'scales_map_df',
    'scales_train_df',
    'scales_transform_df',
    'set_sec_axis',
    'snake_class',
    'split_with_index',
    'stairstep',
    'strip_stage',
    'warn_for_aes_extract_usage',
    'weave_tables_col',
    'weave_tables_row'
  )
)

ggint <- structure(
  mapply(function(.internals, i) getFromNamespace(i, 'ggplot2'), .internals, .internals),
  class = c('internal')
)

new_data_frame <- function (x = list(), n = NULL)
{
  if (length(x) != 0 && is.null(names(x))) {
    abort("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0)
      0
    else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n)
      next
    if (lengths[i] != 1) {
      abort("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }
  class(x) <- "data.frame"
  attr(x, "row.names") <- .set_row_names(n)
  x
}
