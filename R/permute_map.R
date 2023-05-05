#' Permute a double-input function operation
#'
#' @param vec_list_df A vector, list or data frame.
#'
#' @param .f A double input-function.
#' @param ... Additional arguments for the double-input function (if needed).
#'
#' @details
#' This function should be used especially in cases where the input order of the arguments of the double-input function defined in `.f` matters (ex. `-`).
#'   * Functions such as `cor()` and `sum()` are not good options (input order does not matter);
#'   * Dot-dot-dot functions are also allowed (ex. `rbind()`, `cbind()`, etc);
#'   * If possible, functions with a formula input can be modified to a double-input function (`see examples`).
#'
#' @return Returns a list with `n * (n - 1)` elements resulting from the double-input function operation (defined in `.f`) permuted on the `n` elements of the object defined in `vec_list_df`.
#'
#' @seealso
#'    [comb_map()] for combining instead of permuting.
#'
#' @export
#'
#' @importFrom purrr map discard
#'
#' @examples
#' # Load imports
#' library(dplyr)
#'
#' # Operation on a vector (default: `-`)
#' c(a = 1, b = 2, c = 3) |>
#'   permute_map()
#'
#' # Operation on a list (difference between elements)
#' list(a = 1:5, b = 4:10, c = 3:5) |>
#'   permute_map(setdiff)
#'
#' # Operation on a data frame (linear model)
#' lm_quad <- function(y, x) {
#'   lm(y ~ x + I(x^2))
#' }
#'
#' mtcars |>
#'   select_at(vars(disp:drat)) |>
#'   permute_map(lm_quad)
permute_map <- function(vec_list_df, .f = `-`, ...) {

  x_func <- function(y) {
    vec_list_df |>
      map(\(x) if (!setequal(x, y)) .f(y, x, ...)) |>
      discard(is.null)
  }

  vec_list_df |> map(x_func)

}
