#
# fort package - utility functions ----
#

#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
.are_similar_ft <- function(x, y, tolerance = 10^-6) {
  # objects are assumed to have been pre-validated using .is_valid_ft()
  out_val <- FALSE
  attr(out_val, "reason") <- "critical error"
  # only the most important fields are compared, to save time
  fields_to_compare <- c("dim_in", "dim_out", "blocksize", "cache_matrix",
                         "fort_type", "inverse", "invertible", "fwd_par")
  for (i in 1:length(fields_to_compare)) {
    cur_field <- fields_to_compare[i]
    if (!isTRUE(all.equal(x[[cur_field]], y[[cur_field]], tolerance = tolerance))) {
      attr(out_val, "reason") <- paste0("discrepancy in field '",
                                        cur_field, "'")
      return(out_val)
    }
  }
  # all tests passed: objects are similar
  out_val <- TRUE
  attr(out_val, "reason") <- "objects are similar"
  out_val
}

#' Generates a random permutation/expansion/contraction of the appropriate size
#'
#' If dim_in == dim_out, it generates a permutation; if dim_in > dim_out, it generates
#' a contraction; if dim_in < dim_out, it generates an expansion.
#'
#' @param dim_in Number of input elements for permutation
#' @param dim_out Number of output elements for permutation
#'
#' @return A vector of integers representing a permutation/contraction/expansion
#' @noRd
.get_random_permutation <- function(dim_in, dim_out) {
  if (dim_in < dim_out) {
    # expansion
    # dim_out is assumed to be a power of 2
    n_base_reps <- floor(dim_out / dim_in)
    n_additional <- dim_out - n_base_reps * dim_in
    sample(c(
      rep(1:dim_in, n_base_reps),
      sample(1:dim_in, size = n_additional)
    ))
  } else {
    # permutation or contraction
    sample(1:dim_in, size = dim_out)
  }
}

#' Calculate dim_in, dim_out and blocksize from a set of possibly irregular inputs
#'
#' @param dim_in either a scalar or a numeric vector of length 2
#' @param dim_out either a scalar or, if dim_in is a vector, NULL
#' @param min_blocksize a scalar indicating the minimum blocksize wanted
#'
#' @return a list with fields 'dim_in', 'dim_out' and 'blocksize'
#' @noRd
.get_dims_from_inputs <- function(dim_in, dim_out = NULL, min_blocksize = 0) {
  # validate and determine dim_in and dim_out
  if (!is.numeric(dim_in)) stop("the first argument of fort() must be numeric")
  # determine dim_in and dim_out
  dims <- c(dim_in, dim_in)
  if (is.null(dim_out)) {
    # take both values from dim_in
    dim_in_ <- dims[1]
    dim_out_ <- dims[2]
  } else {
    if (!is.numeric(dim_out)) {
      stop("the second argument of fort() must be numeric")
    }
    dim_in_ <- dim_in[1]
    dim_out_ <- dim_out[1]
  }
  if (!((dim_in_ > 0) && (dim_out_ > 0))) {
    stop("dim_in and dim_out must be positive when calling fort()")
  }
  # define blocksize based on dim_in, dim_out and min_blocksize
  # (making sure it is a power of 2 and above 2)
  blocksize <- 2^ceiling(log(max(2, dim_in_, dim_out_, min_blocksize, na.rm = TRUE)) / log(2))
  # return validated inputs
  list(dim_in = dim_in_, dim_out = dim_out_, blocksize = blocksize)
}

#' Get an appropriate constructor function for a certain fort type
#'
#' The input (fort_type) should be either a constructor function, an R6ClassGenerator for
#' a class that inherits from FastTransform, a string that indicates such a class, or a string
#' indicating a "friendly" name for one of the available methods.
#'
#' @param fort_type string, function or R6ClassGenerator
#'
#' @return constructor function that can be called to obtain an object of type fort_type
#' @noRd
.get_fort_constructor <- function(fort_type) {
  available_methods <- c("default", "fft2")
  if (is.character(fort_type)) {
    switch(fort_type[1],
      default = FastTransformFFT2$new,
      fft2 = FastTransformFFT2$new,
      {
        error_message <- paste0(
          "'",fort_type[1],"'",
          " is not a valid value for the 'type' field when",
          " calling fort(); please choose one of: ",
          paste0(available_methods, collapse = ", ")
        )
        # check if it is the name of an existing FastTransform method
        if (exists(fort_type[1])) {
          # the object exists, so get it...
          target_obj <- get(fort_type[1])
          # ...and check if it is an R6ClassGenerator
          if (!inherits(target_obj, "R6ClassGenerator")) stop(error_message)
          # if it is, pass its constructor function
          return(target_obj$new)
        } else {
          stop(error_message)
        }
      }
    )
  } else {
    if (is.numeric(fort_type)) {
      error_message <- paste0(
        "'",fort_type[1],"'",
        " is not a valid value for the 'type' field when",
        " calling fort(); please choose one of: ",
        paste0(available_methods, collapse = ", ")
      )
      stop(error_message)
    } else {
      if (is.function(fort_type)) {
        # input is a function: assume it is a valid constructor function, and just return it
        fort_type
      } else {
        if (inherits(fort_type, "FastTransform")) {
          # input is a FastTransform; return its constructor function
          fort_type$new
        } else {
          error_message <- paste0(
            "invalid value provided for the 'type' field when",
            " calling fort(); please choose one of: ",
            paste0(available_methods, collapse = ", ")
          )
          stop(error_message)
        }
      }
    }
  }
}

#' Get a set of symbols to be used when printing FastTransform objects
#'
#' Note: unless 'options(fort.avoid_unicode = TRUE)' is called, UTF-8 symbols will be used by default
#'
#' @param simple logical that indicates whether UTF-8 (FALSE) or 7-bit ASCII (TRUE) symbols are used
#'
#' @return list with symbols to be used
#' @noRd
.get_printing_symbols <- function(simple = NULL) {
  if (is.null(simple)) {
    # unless 'options(fort.avoid_unicode = TRUE)' is called,
    # the printing symbols will use UTF-8
    simple_ <- getOption("fort.avoid_unicode")
    if (is.null(simple_)) simple_ <- FALSE
  } else {
    simple_ <- simple
  }
  if (simple_) {
    # use 7-bit ASCII
    s_list <- list(
      arrows = c("->", "<-"),
      real = "R", complex = "C",
      times = "x"
    )
  } else {
    # use UTF-8
    s_list <- list(
      arrows = c("\u2192", "\u2190"),
      real = "\u211d", complex = "\u2102",
      times = "\u00d7"
    )
  }
  s_list
}

#' Function that provides a generalized inverse of a (complex) scaling vector
#'
#' @param scaling_vector complex vector to (pseudo)invert
#'
#' @return a vector of the same length as scaling_vector
#' @noRd
.pseudoinvert_scaling <- function(scaling_vector) {
  # calculates "the" pseudoinverse of a scaling vector
  # also works for complex scalings (i.e. rotations)
  x_ <- 1 / scaling_vector # just invert element-wise
  # ensure that zeros get mapped back to a zero
  is_zero <- (!(Mod(scaling_vector) > 0)) | (!is.finite(x_))
  if (any(is_zero)) x_[is_zero] <- 0
  x_
}

#' Function that provides a generalized inverse of a permutation
#'
#' NA in inputs is interpreted as "unknown"
#'
#' @param permutation vector of integers to (pseudo)invert
#'
#' @return a vector of the same length as scaling_vector
#' @noRd
.pseudoinvert_permutation <- function(permutation) {
  # calculates "the" pseudoinverse of a permutation
  # also works for expansions and contractions
  if (any(is.na(permutation))) {
    # the permutation is actually a contraction
    # inversion in the true sense is impossible (just provide a pseudoinverse)
    is_good <- !is.na(permutation)
    good_idx <- (1:length(permutation))[is_good]
    other_idx <- (1:length(permutation))[!is_good]
    good_values <- permutation[!is.na(permutation)]
    other_values <- (1:length(permutation))
    other_values <- other_values[!(other_values %in% good_values)]
    p_ <- permutation
    p_[good_values] <- good_idx
    p_[other_values] <- other_idx
  } else {
    if (length(unique(permutation)) < length(permutation)) {
      # the permutation is actually an expansion
      # so just take the minimal required set
      p_size <- length(unique(permutation))
      p_ <- rep(NA, p_size)
      for (i in 1:p_size) {
        target_idx <- ((1:length(permutation))[permutation == i])[1] # get first option
        p_[i] <- target_idx # assign it
      }
    } else {
      # the permutation is actually a permutation, so just use the sorting method
      p_ <- sort.int(permutation, index.return = TRUE)$ix
    }
  }
  p_
}
