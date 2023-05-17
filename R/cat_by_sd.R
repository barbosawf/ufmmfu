#' Categorize a vector according to standard deviation values
#'
#' @param x A numeric vector.
#' @param n A number that multiplies the standard deviation.
#' @param categories Categories used to classify the numeric elements in x. It must be of length 3. Default is NULL.
#' @param na.resp Response when `NA` is found. Default is `NA`.
#'
#' @importFrom dplyr case_when
#' @importFrom stats sd
#' @return A character vector containing the categories used to classify the values of the original vector based on the quantiles defined by the proportionality of the standard error.
#' @export
#'
#' @examples
#' set.seed(123)
#' s <- rnorm(20)
#'
#' cat_by_sd(s)
#'
#' cat_by_sd(s, categories = c("low", "mid", "high"))
#'
#' cat_by_sd(s, n = 0.5)
#'
#' s[1:5] <- NA
#'
#' cat_by_sd(s)
#'
#' cat_by_sd(s, na.resp = "nope")
cat_by_sd <- function(x, n = 1, categories = NULL, na.resp = NA) {
  if (is.null(categories)) {
    categories = c('Below', 'Between', 'Above')
  } else {
    cat('WATCH OUT! Catogories must be in order!\n')
  }

  if (length(categories) != 3) {
    stop('WATCH OUT! The length of categories must be 3!\n')
  } else {
    if (length(x) < 3 | !is.numeric(x)) {
      stop('WATCH OUT! The length of x must be higher than 3 and x must be a numeric vector!\n')
    } else {
      if (n <= 0) {
        n <- 1
        cat('WATCH OUT! \n
          The number (n) that multiplies de standard error was defined as 1.\n')
      }

      li <- mean(x, na.rm = TRUE) - n * sd(x, na.rm = TRUE)
      ls <- mean(x, na.rm = TRUE) + n * sd(x, na.rm = TRUE)
      case_when(
        x <  li ~ categories[1],
        x >= li & x <= ls ~ categories[2],
        x > ls ~ categories[3],
        is.na(x) ~ na.resp
      )

    }
  }

}

