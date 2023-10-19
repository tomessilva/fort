#
# fort package - FastTransformFFT1 class definition ----
#

#' `FastTransformFFT1` subclass
#'
#' A specific implementation of a structured fast transform. Inherits from [FastTransform].
#'
#' In particular, the `fft1` type applies the following set of operations to each input (column) vector:
#'
#' \enumerate{
#'   \item Permute/expand (\eqn{P_1}) rows and pack them into a complex vector \eqn{x};
#'   \item Apply a \eqn{y = D_2 F D_1 x} linear transform, where \eqn{F} represents a complex FFT,
#'   and \eqn{D_i} represent diagonal matrices of random unitary complex values;
#'   \item Unpack complex vector \eqn{y} to real vector and permute/contract (\eqn{P_2}) rows.
#' }
#'
#' Note that this transform will be orthonormal only when \eqn{dim\_in = dim\_out = blocksize} (in which case,
#' both \eqn{P_1} and \eqn{P_2} are permutations).
#'
#' Otherwise, when \eqn{dim\_in < blocksize}, \eqn{P_1} represents an expansion (rather than a permutation),
#' and when \eqn{dim\_out < blocksize}, \eqn{P_2} represents a contraction/decimation (rather than a
#' permutation). When both of these conditions are true, the resulting transform will \emph{not} be exactly
#' orthogonal or semi-orthogonal, but the rows and columns of the transform are still going to be generally
#' uncorrelated.
#'
#' It is \strong{not} recommended that the methods described below are called directly. Instead,
#' use the methods described in the [fort()] documentation, if possible, unless you positively need
#' low-level access (e.g., to speed up computation on \emph{pre-validated} inputs).
#'
#' @seealso [fort()], [FastTransform]
#' @export
FastTransformFFT1 <- R6::R6Class(
  classname = "FastTransformFFT1",
  inherit = FastTransform,
  public = list(

    #' @description Object creation function. It is recommended
    #'   to call the `fort()` function with `type = "FastTransformFFT1"`,
    #'   instead of this method, since \emph{no input validation is
    #'   performed by this method}.
    #' @param dim_in Dimensionality of the input for the forward transform.
    #' @param dim_out Dimensionality of the output for the forward transform.
    #' @param blocksize Dimensionality of the internal transformation
    #'   (\emph{must} be a power of 2).
    #' @return A matrix with the same number of columns as `x`.
    initialize = function(dim_in, dim_out, blocksize) {
      # init fwd_par
      out_list <- list()
      # define initial permutation/expansion - p1
      p1 <- .get_random_permutation(dim_in, blocksize)
      # separate into real and imaginary parts
      # note: length of p1 is necessarily even
      nn <- blocksize / 2
      out_list[["p1_re"]] <- p1[1:nn]
      out_list[["p1_im"]] <- p1[(nn + 1):blocksize]
      # define complex scaling parameters - cs1, cs2
      # generate random unitary isotropic complex numbers
      cs1 <- complex(modulus = 1, argument = 2 * pi * runif(nn))
      # the scale of the last one is corrected
      # to ensure norm-preservation
      overall_scaling_factor <- sqrt(dim_in / (nn * dim_out))
      cs2 <- complex(
        modulus = 1,
        argument = 2 * pi * runif(nn)
      ) * overall_scaling_factor
      out_list[["cs1_re"]] <- Re(cs1)
      out_list[["cs1_im"]] <- Im(cs1)
      out_list[["cs2_re"]] <- Re(cs2)
      out_list[["cs2_im"]] <- Im(cs2)
      # define final permutation/contraction - p2
      out_list[["p2"]] <- .get_random_permutation(blocksize, dim_out)
      self$fwd_par <- out_list

      # init rest of fields
      self$inverse <- FALSE
      self$dim_in <- dim_in
      self$dim_out <- dim_out
      self$blocksize <- blocksize
      self$fort_type <- "fft1"

      # check if transform is invertible
      self$invertible <- (self$dim_in == self$dim_out) &&
        (self$dim_in == self$blocksize)
    },

    #' @description Function that performs the forward transform.
    #'   Do \emph{not} call this directly unless you know what you are doing:
    #'   use the [`%*%.FastTransform`] or `FastTransform$evaluate()`
    #'   methods instead.
    #' @param x Input matrix of the \emph{correct} dimensionality
    #' @return A matrix with the same number of columns as `x`.
    fwd_eval = function(x) {
      p_ <- self[["fwd_par"]]
      # initial permutation/expansion
      X1 <- x[p_[["p1_re"]], , drop = FALSE]
      X2 <- x[p_[["p1_im"]], , drop = FALSE]
      # apply transform (call external C++ function)

      Xout <- .Call(`_fort_FORT_CPP_FFT1_fwd`,
        # X_Re, X_Im
        X1, X2,
        # s1, s2, s3, s4
        p_[["cs1_re"]], p_[["cs1_im"]],
        p_[["cs2_re"]], p_[["cs2_im"]],
        PACKAGE = "fort"
      )

      # final permutation/contraction
      Xout[p_[["p2"]], , drop = FALSE]
    },

    #' @description Function that performs the inverse transform.
    #'   Do \emph{not} call this directly unless you know what you are doing:
    #'   use the [`%*%.FastTransform`] or `FastTransform$evaluate()`
    #'   methods instead.
    #' @param x Input matrix of the \emph{correct} dimensionality
    #' @return A matrix with the same number of columns as `x`.
    rev_eval = function(x) {
      p_ <- self$rev_par
      # initial permutation/expansion
      in_dim <- nrow(x)
      blocksize <- self$blocksize
      if (in_dim < blocksize) {
        # expansion of input matrix with zeros required
        x_ <- rbind(x, matrix(0, nrow = blocksize - in_dim, ncol = ncol(x)))
        X1 <- x_[p_[["p2i_re"]], , drop = FALSE]
        X2 <- x_[p_[["p2i_im"]], , drop = FALSE]
        rm(x_)
      } else {
        X1 <- x[p_[["p2i_re"]], , drop = FALSE]
        X2 <- x[p_[["p2i_im"]], , drop = FALSE]
      }
      # apply transform (call external C++ function)
      Xout <- .Call(`_fort_FORT_CPP_FFT1_rev`,
        # X_Re, X_Im
        X1, X2,
        # s1, s2, s3, s4
        p_[["cs2i_re"]], p_[["cs2i_im"]],
        p_[["cs1i_re"]], p_[["cs1i_im"]],
        PACKAGE = "fort"
      )

      # final permutation/contraction
      Xout[p_[["p1i"]], , drop = FALSE]
    },

    #' @description Function that calculates and caches the parameters
    #'   for the inverse transform. Do not call this directly unless you
    #'   know what you are doing. If you need the inverse transform, use
    #'   the [`solve.FastTransform`] or `FastTransform$get_inverse()`
    #'   methods instead.
    #' @return The object itself (invisibly).
    calculate_rev_par = function() {
      # original has seven parameters:
      #  * p1_re, p1_im
      #  * cs1_re, cs1_im, cs2_re, cs2_im
      #  * p2
      # inverse has seven parameters:
      #  * p2i_re, p2i_im
      #  * cs2i_re, cs2i_im, cs1i_re, cs1i_im
      #  * p1i
      o_ <- self$fwd_par # original parameters
      # first take p1_re and p1_im and combine them
      p1 <- c(o_[["p1_re"]], o_[["p1_im"]])
      # pseudoinvert the first permutation
      p1i <- .pseudoinvert_permutation(p1)
      # get the scaling parameters
      cs1 <- complex(real = o_[["cs1_re"]], imaginary = o_[["cs1_im"]])
      cs2 <- complex(real = o_[["cs2_re"]], imaginary = o_[["cs2_im"]])
      # pseudoinvert scaling parameters
      cs1i <- .pseudoinvert_scaling(cs1)
      cs2i <- .pseudoinvert_scaling(cs2)
      # pseudoinvert the last permutation
      p2_pre <- o_[["p2"]] # get permutation
      blocksize <- length(cs1i) * 2 # 2x the size of scaling variables
      p2 <- rep(NA, blocksize)
      # ensure that, if the permutation is a contraction, it has NAs
      p2[1:length(p2_pre)] <- p2_pre
      p2i <- .pseudoinvert_permutation(p2)
      # separate it into the real and imaginary components
      nn <- length(p2i) / 2 # length is even
      p2i_re <- p2i[1:nn]
      p2i_im <- p2i[(nn + 1):length(p2i)]
      # set parameters for inverse function
      self$rev_par <- list(
        p2i_re = p2i_re, p2i_im = p2i_im,
        cs2i_re = Re(cs2i), cs2i_im = Im(cs2i),
        cs1i_re = Re(cs1i), cs1i_im = Im(cs1i),
        p1i = p1i
      )
      # return self
      invisible(self)
    }
  )
)
