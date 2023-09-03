#
# fort package - imports/exports ----
#

#' @useDynLib fort, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom R6 R6Class
#' @importFrom MASS ginv
#' @exportPattern "^[[:alpha:]]+"
NULL
