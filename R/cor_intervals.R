#' Frequency of sliced correlation range.
#'
#' @param x A correlation matrix.
#' @param absolute A boolean value (`True` or `FALSE`). If `FALSE` (default) returns a correlation interval between -1 and 1. If `TRUE` computes absolute correlations and therefore returns a range between 0 and 1.
#' @param inter A value (0 - 0.5] used to cut the correlation interval. Default is 0.2.
#'
#' @return A data frame with the frequencies and rates of the sliced correlation range.
#'
#' @import correlation
#' @importFrom dplyr mutate
#'
#' @export
#'
#' @examples
#' corr_mtcars <- cor(mtcars)
#'
#' cor_intervals(corr_mtcars)
#'
#' cor_intervals(corr_mtcars, absolute = TRUE, inter = 0.1)
#'
cor_intervals <- function(x, absolute = FALSE, inter = 0.2) {

  if (!correlation::is.cor(x)) {
    cat('x is not a correlation matrix!')
  } else if (!is.logical(absolute) & length(absolute)) {
    cat('absolute is not a single logical value!')
  } else if (!is.numeric(inter)) {
    cat('inter must be numerical!')
  } else if (!inter > 0 & !inter <= 0.5) {
    cat('inter must be higher than zero and equal or lower than 0.5')
  } else {

    ll <- -1

    if (absolute) {
      x <- abs(x)
      ll <- 0
    }

    x |>
      upper.tri(diag = T) -> x_upper_tri

    x[x_upper_tri] <- NA

    x |>
      as.table() |>
      as.data.frame() |>
      na.omit() -> x_lower_tri_NA

    x_lower_tri_NA[, 3] |>
      cut(seq(from = ll,
              to = 1,
              by = inter),
          include.lowest = T) |>
      table() |>
      data.frame(n = dim(x_lower_tri_NA)[1]) |>
      setNames(c('Interval', 'Frequency', 'n')) |>
      dplyr::mutate(Rate = round(Frequency/n, 4))
  }

}
