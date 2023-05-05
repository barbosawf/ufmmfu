#' Remove the diagonal from a matrix and collapse the remaining elements
#'
#' @description
#' This function removes the diagonal from a square matrix and collapses the remaining elements.
#'
#' @param x a square matrix.
#' @param by accepts only "row" or "col".
#'
#' @return Returns a non-square matrix of dimension (n - 1 x n) or (n x n - 1) if `by` is respectively set to "row" or "col".
#' @export
#'
#' @examples
#' # Defining a matrix object
#' set.seed(123)
#' m <- matrix(sample(1:9), 3, 3)
#' m
#'
#' select_all_but_diag(x = m, by = "row")
#'
#' select_all_but_diag(x = m, by = "col")
select_all_but_diag <- function(x, by = "row") {

  if (!is.matrix(x)) {
    stop('x is not a matrix!')
  } else if (!setequal(dim(x)[1],dim(x)[2])){
    stop('x is not a square matrix!')
  } else if (!by  %in% c("row" , "col")) {
    stop("collapse_by accepts only 'row' or 'col'.")
  } else {

    aux_x <- diag(x = 1, nrow = nrow(x), ncol = ncol(x))

    if (by == "row") {

      matrix(x[!aux_x],
             nrow = nrow(x) - 1,
             ncol = ncol(x)
      )

    }
    else if (by == "col") {

      t(matrix(t(x)[!aux_x],
               nrow = nrow(x) - 1,
               ncol = ncol(x)
      ))
    }

  }
}
