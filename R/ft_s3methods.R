#
# fort package - S3 convenience functions for FastTransform objects ----
#

# define methods for FastTransform class objects

#' Apply a fast transform
#'
#' Applies a fast transform created by `fort()` (`x`) to the columns of a conformable matrix (`y`).
#'
#' @param x An object of class `FastTransform`, created using `fort()`.
#' @param y A numeric (real) matrix/vector with an appropriate number of rows/elements.
#'
#' @return A numeric (real) matrix with the same number of columns as `y`.
#' @export
#'
#' @examples
#' Z <- fort(4, 1024)
#' Z %*% matrix(1:2, 4, 3) # output is a 1024 by 3 matrix
#' # the example below works: y is assumed to be a single column vector
#' Z %*% 1:4 # output is a 1024 by 1 matrix
`%*%.FastTransform` <- function(x, y) {
  x$evaluate(y)
}

#' Dimensions of fast transform
#'
#' Retrieves the dimensions of a fast transform created by `fort()` (i.e., the number of rows and
#' columns of an equivalent matrix). It returns the same value that one would get from `dim(as.matrix())`,
#' but much more efficiently.
#'
#' @param x An object of class `FastTransform`, created using `fort()`.
#'
#' @return A vector of length 2 containing the dimensions of the fast transform (i.e., number of rows and
#'   number of columns, in this order).
#' @export
#'
#' @examples
#' dim(fort(3, 17)) # should return c(17,3)
#' dim(t(fort(3, 17))) # should return c(3,17)
dim.FastTransform <- function(x) {
  x$get_dim()
}

#' Convert fast transform to matrix
#'
#' Converts a fast transform created by `fort()` to the equivalent matrix form.
#'
#' @param x An object of class `FastTransform`, created using `fort()`.
#' @param ... Extra parameters (ignored).
#'
#' @return A `matrix` object equivalent to x.
#' @export
#'
#' @examples
#' fast_transform <- fort(4, 15)
#' slow_transform <- as.matrix(fast_transform)
#' fast_result <- fast_transform %*% diag(4)
#' slow_result <- slow_transform %*% diag(4)
#' norm(fast_result - slow_result) # should be small
as.matrix.FastTransform <- function(x, ...) {
  x$as_matrix()
}

#' Summarize fast transform
#'
#' Provides a summary of a fast transform created by `fort()` with slightly more detail than the
#' information provided by using `print()`.
#'
#' @param object An object of class `FastTransform`, created using `fort()`.
#' @param ... Extra parameters (ignored).
#'
#' @return The input object (invisibly).
#' @export
#'
#' @examples
#' summary(fort(3, 17))
summary.FastTransform <- function(object, ...) {
  object$summary()
}

#' Calculate the Determinant of a Transform
#'
#' `det` calculates the determinant of a FastTransform object. `determinant` returns separately the
#' modulus of the determinant, optionally (by default) on the logarithm scale, and the sign of the
#' determinant. If the input transform (`x`) is not square, the function will fail with an error.
#'
#' @param x Object of `FastTransform` type with `dim_in == dim_out`.
#' @param logarithm Logical. if `TRUE` (default) return the logarithm of the modulus of the determinant.
#' @param ... Extra parameters (ignored).
#'
#' @return For `det`, the determinant of `x`. For `determinant`, the same output format as
#'   `determinant.matrix()`.
#' @export
#'
#' @examples
#' det(fort(16)) # either 1 or -1
#' determinant(fort(16))
determinant.FastTransform <- function(x, logarithm = TRUE, ...) {
  logdet <- x$get_logdet()
  if (!logarithm) {
    logdet$modulus <- exp(logdet$modulus)
    attr(logdet$modulus, "logarithm") <- FALSE
  }
  logdet
}

#' Transform Transpose
#'
#' Given a `FastTransform` object `x`, `t` returns the transpose of `x`. If `x` represents an orthonormal
#' transformation (i.e., if `x$invertible` is `TRUE`), then a `FastTransform` object (representing
#' the transpose of `x`) will be returned; otherwise, a matrix object (representing the transpose of `x`)
#' will be returned, with a warning.
#'
#' @param x An object of class `FastTransform`, created using `fort()`.
#'
#' @return Either an object of class `FastTransform` (if `x$invertible` is `TRUE`) or a matrix.
#' @export
#'
#' @examples
#' (a <- fort(4))
#' (b <- t(t(a))) # transpose a twice
#' # the result below should be close to zero
#' sum((a %*% diag(4) - b %*% diag(4))^2)
t.FastTransform <- function(x) {
  if (!x$invertible) {
    # warn user
    warning("t() converted FastTransform object into a matrix")
  }
  x$get_transpose()
}

#' Solve a System of Equations
#'
#' Solves an equation of the form `a %*% x = b` for `x`, where `a` is a linear operation represented by
#' a `FastTransform` object, while `b` can be either a vector or a matrix. If `b` is missing, it returns
#' a `FastTransform` object corresponding to the inverse (or a generalized inverse) of `a`.
#'
#' @param a An object of class `FastTransform`, created using `fort()`.
#' @param b A numeric vector or matrix (to solve the equation), or nothing (to obtain a generalized
#'   inverse of `a`).
#' @param ... Extra parameters (ignored).
#'
#' @return Either a matrix (representing `x`), or a `FastTransform` object (representing a generalized
#'   inverse of `a`; if parameter `b` is missing).
#' @export
#'
#' @examples
#' a <- fort(4)
#' inv_a <- solve(a) # inverse of a
#' inv_a %*% diag(4) # applying the inverse of a
#' solve(a, diag(4)) # should give the same output
solve.FastTransform <- function(a, b, ...) {
  # get generalized inverse
  inverse_transform <- a$get_inverse()
  if (missing(b)) {
    # return it
    inverse_transform
  } else {
    # solve for b by using it directly on b
    inverse_transform %*% b
  }
}
