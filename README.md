
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fort ~ fast orthogonal random transforms in R

<!-- badges: start -->

<figure>
<img src="https://img.shields.io/badge/package-fort-blue?logo=r"
alt="R package - fort" />
<figcaption aria-hidden="true">R package - fort</figcaption>
</figure>

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

This is a basic example which shows how to use `fort` in practice:

``` r
library(fort)

(fast_transform <- fort(4)) # fast orthogonal transform from R^4 to R^4
#> fort linear operation: R^4 -> [fft2] -> R^4

matrix_to_transform <- diag(4) # 4 x 4 identity matrix
(new_matrix <- fast_transform %*% matrix_to_transform) # transformed matrix
#>             [,1]       [,2]        [,3]        [,4]
#> [1,] -0.88213943 0.45899830  0.05379707  0.09086504
#> [2,]  0.37597596 0.81637368 -0.34635307 -0.26872968
#> [3,]  0.09086504 0.05379707 -0.45899830  0.88213943
#> [4,]  0.26872968 0.34635307  0.81637368  0.37597596

(inverse_transform <- solve(fast_transform)) # get inverse transform
#> fort linear operation (inverted): R^4 <- [fft2] <- R^4

round(inverse_transform %*% new_matrix, 12) # should recover the identity matrix
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    0    0    0
#> [2,]    0    1    0    0
#> [3,]    0    0    1    0
#> [4,]    0    0    0    1
```

Here is a comparison of using a `fort` transform against a simple matrix
multiplication, in terms of speed:

``` r
library(fort)

matrix_to_transform <- diag(1024) # 1024 x 1024 identity matrix

fast_transform <- fort(1024) # fast orthogonal transform from R^1024 to R^1024
slow_transform <- as.matrix(fast_transform) # the same, but in matrix form

# time it takes for the fast transform
system.time(for (i in 1:100) test <- fast_transform %*% matrix_to_transform, gcFirst = TRUE)
#>   user  system elapsed 
#>   5.50    2.12    8.00

# time it takes for the equivalent slow transform (via matrix multiplication)
system.time(for (i in 1:100) test <- slow_transform %*% matrix_to_transform, gcFirst = TRUE)
#>   user  system elapsed 
#>  70.57    0.61   77.95 
```
