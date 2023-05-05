#' Combine a double-input function operation
#'
#' @inheritParams permute_map
#'
#' @details
#' This function should be used especially in cases where the input order of the arguments of the double-input function defined in `.f` DOES NOT matters (ex. `+`).
#'   * Functions such as `setdiff()` is not a good option (input order matters);
#'   * Dot-dot-dot functions are also allowed (ex. `sum()`);
#'   * If possible, functions with a formula input can be modified to a double-input function (`see examples`).
#'
#' @return Returns a list with `[n * (n - 1)]/2` elements resulting from the double-input function operation (defined in `.f`) combining the `n` elements of the object defined in `vec_list_df`.
#'
#' @seealso
#'    [permute_map()] for permuting instead of combining.
#'
#' @export
#'
#' @importFrom purrr map map2
#' @importFrom stats setNames
#' @importFrom utils combn
#'
#' @examples
#' # Load imports
#' library(dplyr)
#'
#' # Operation on a vector (default: `+`)
#' c(a = 1, b = 2, c = 3, d = 4) |>
#'   comb_map()
#'
#' # Operation on a list (intersection between elements)
#' list(a = 1:5, b = 4:10, c = 3:5, d = 8:12) |>
#'   comb_map(intersect)
#'
#' # Operation on a data frame (linear model)
#' lm_mod <- function(y, x) {
#'   lm(y ~ x)
#' }
#'
#' mtcars |>
#'   select_at(vars(disp:wt)) |>
#'   comb_map(lm_mod)
comb_map <- function(vec_list_df, .f = `+`, ...) {

  l <- length(vec_list_df)

  combs <- combn(l, 2)

  map2(
    seq_len(dim(combs)[2]) |>
      map(\(x) vec_list_df[[combs[1 ,][x]]]),

    seq_len(dim(combs)[2]) |>
      map(\(x) vec_list_df[[combs[2 ,][x]]]), .f, ...) -> out

  if (!is.null(names(vec_list_df)) &
      length(vec_list_df) == length(na.omit(names(vec_list_df)))) {

    out |>
      set_names(names(vec_list_df)[combs[2,]]) |>
      split(names(vec_list_df)[combs[1,]]) -> out
  }

  if (length(vec_list_df) != length(na.omit(names(vec_list_df)))) {
    cat('The output list is unnamed if not all of the input list is also unnamed. \n\n')
  }

  out

}
