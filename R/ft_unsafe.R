#
# fort package - unsafe operator (%***%) ----
#

#' Unsafely apply a fast transform
#'
#' Applies a fast transform created by `fort()` (`x`) to the columns of a conformable matrix (`y`),
#' typically equivalent to the use of the `%*%` operator, but using an \strong{unsafe} method.
#'
#' This operator works in a similar way to `%*%`, but avoids dispatching and does not perform any type of
#' validation of its inputs, in order to reduce overhead when performing repeated operations inside a
#' function on \emph{pre-validated} inputs.
#'
#' It is \emph{not} recommended that this operator is used interactively and/or on non-validated inputs.
#'
#' @param x An object of class `FastTransform`, created using `fort()`.
#' @param y A numeric (real) matrix/vector with an appropriate number of rows/elements.
#'
#' @return A numeric (real) matrix with the same number of columns as `y`.
#'
#' @seealso [`fort()`], [`%*%.FastTransform`]
#'
#' @export
#'
#' @examples
#' Z <- fort(4, 1024)
#' Z %*% matrix(1:2, 4, 3) # output is a 1024 by 3 matrix
#' Z %***% matrix(1:2, 4, 3) # output is also a 1024 by 3 matrix
`%***%` <- function(x, y) {
  # note: no validation whatsoever of either of the inputs
  if (x$inverse) {
    # evaluate inverse
    if (x$invertible) {
      # evaluate inverse normally
      x$rev_eval(y)
    } else {
      # use matrix form to calculate inverse
      x$as_matrix() %*% y
    }
  } else {
    # evaluate transform normally
    x$fwd_eval(y)
  }
}
