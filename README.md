
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fort ~ fast orthogonal random transforms in R

<!-- badges: start -->
<!-- badges: end -->

The `fort` package provides convenient access to fast structured random
linear transforms that are (at least approximately) orthogonal or
semi-orthogonal, and generally faster than matrix multiplication (in the
style of the Fastfood Transform), implemented in C++ (via Rcpp).

Useful for algorithms that require or benefit from uncorrelated random
projections, such as fast dimensionality reduction (e.g.,
Johnson-Lindenstrauss transform) or kernel approximation (e.g., random
kitchen sinks).

## Installation

You can install the development version of `fort` from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tomessilva/fort")
```

## Example

This is a basic example which shows you how to use `fort` in practice:

``` r
library(fort)

(fast_transform <- fort(4)) # fast orthogonal transform from R^4 to R^4
#> fort linear operation: R^4 -> [fft2] -> R^4
matrix_to_transform <- diag(4) # 4 x 4 identity matrix
(new_matrix <- fast_transform %*% matrix_to_transform) # transformed matrix
#>              [,1]        [,2]         [,3]        [,4]
#> [1,]  0.307407541 -0.10261378  0.944572587  0.05247517
#> [2,] -0.944572587  0.05247517  0.307407541  0.10261378
#> [3,] -0.115222662 -0.68752202  0.002639952 -0.71695902
#> [4,] -0.002639952 -0.71695902 -0.115222662  0.68752202
(inverse_transform <- solve(fast_transform)) # get inverse transform
#> fort linear operation (inverted): R^4 <- [fft2] <- R^4
round(inverse_transform %*% new_matrix,16) # should recover the identity matrix
#>        [,1]   [,2]   [,3]   [,4]
#> [1,]  1e+00 -2e-16 -1e-16  3e-16
#> [2,] -1e-16  1e+00  1e-16 -2e-16
#> [3,]  1e-16  3e-16  1e+00  2e-16
#> [4,]  1e-16  2e-16  1e-16  1e+00
```
