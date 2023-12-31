% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ft_s3methods.R
\name{determinant.FastTransform}
\alias{determinant.FastTransform}
\title{Calculate the Determinant of a Transform}
\usage{
\method{determinant}{FastTransform}(x, logarithm = TRUE, ...)
}
\arguments{
\item{x}{Object of \code{FastTransform} type with \code{dim_in == dim_out}.}

\item{logarithm}{Logical. if \code{TRUE} (default) return the logarithm of the modulus of the determinant.}

\item{...}{Extra parameters (ignored).}
}
\value{
For \code{det}, the determinant of \code{x}. For \code{determinant}, the same output format as
\code{determinant.matrix()}.
}
\description{
\code{det} calculates the determinant of a FastTransform object. \code{determinant} returns separately the
modulus of the determinant, optionally (by default) on the logarithm scale, and the sign of the
determinant. If the input transform (\code{x}) is not square, the function will fail with an error.
}
\examples{
det(fort(16)) # either 1 or -1
determinant(fort(16))
}
\seealso{
\code{\link[=fort]{fort()}}
}
