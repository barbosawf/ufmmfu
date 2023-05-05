#' Find redundant variables in a correlation matrix
#'
#' This function returns the pairs of redundant variables defined by those that are greater than or equal to the inf_limit.
#'
#' @param x A correlation matrix of a given data frame.
#' @param inf_limit A numeric value indicating the correlation coefficient threshold (default = 1).
#'
#' @return A data frame with columns 'Var1', 'Var2', and 'Correlation' containing pairs of redundant variables and their correlation coefficients.
#'
#' @importFrom dplyr arrange
#' @import magrittr
#'
#' @export
#'
#' @examples
#' # Load packages
#' library(dplyr)
#'
#' # Creating a data frame with highly correlated variables.
#' set.seed(123)
#' mtcars |>
#'     select_at(vars(disp:qsec)) |>
#'     add_column(mtcars |>
#'         select_at(vars(disp:qsec)) |>
#'         mutate_all(~. + rnorm(1, sd = 1))) |>
#'     cor() -> corr
#'
#' # Finding redundancy
#' corr |> redundancy()
#'
#' corr |> redundancy(inf_limit = 0.95)
#'
redundancy <- function(x, inf_limit = 1) {
  x_upper_tri <- upper.tri(x, diag = TRUE)
  x[x_upper_tri] <- NA

  x_lower_tri_NA <- x %>%
    as.table() %>%
    as.data.frame() %>%
    na.omit() %>%
    setNames(c('Var1', 'Var2', 'Correlation'))

  founded_lines <- which(abs(x_lower_tri_NA[, 3]) >= inf_limit)

  if (length(founded_lines) > 0) {
    out <- x_lower_tri_NA[founded_lines,] %>%
      arrange(desc(abs(Correlation)))
  } else {
    out <- NULL
  }

  out
}
