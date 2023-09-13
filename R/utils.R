#
# fort package - utility functions ----
#

#' Get list of available fort types
#'
#' Takes the default list of available fort types and
#' adds any other types defined in .Options$fort.type_list
#'
#' The format (of both .Options$fort.type_list) and the
#' return value of this function is a *named* list, where each
#' element is a character vector of length 1 indicating the name
#' of the corresponding `FastTransform` subclass.
#'
#' @return A named list of available fort types.
#' @noRd
.get_available_fort_types <- function() {
  base_types <- list(
    # here is the default list of available methods
    default = "FastTransformFFT2",
    fft1 = "FastTransformFFT1",
    fft2 = "FastTransformFFT2"
  )
  if ("fort.type_list" %in% names(.Options)) {
    # extra types have been defined by the user
    # add extra fort types to base list
    # overwriting existing ones, if needed
    new_types <- getOption("fort.type_list")
    if (!is.list(new_types)) {
      stop(
        "malformed .Options$fort.type_list",
        " (needs to be a list)"
      )
    }
    for (i in 1:length(new_types)) {
      cur_type <- names(new_types)[i]
      cur_value <- new_types[[i]]
      if (!is.character(cur_value)) {
        stop(
          "malformed field '", cur_type,
          "' in .Options$fort.type_list",
          " (needs to be of character type)"
        )
      }
      if (cur_value == "FastTransform") {
        stop(
          "the general `FastTransform`",
          " class cannot be used in",
          " .Options$fort.type_list"
        )
      }
      base_types[[cur_type]] <- cur_value
    }
  }
  base_types
}

#' Test whether two FastTransform objects are roughly similar
#'
#' Only compares a subset of all fields, for efficiency.
#'
#' @param x A pre-validated FastTransform object.
#' @param y Another pre-validated FastTransform object.
#' @param tolerance Numeric that defines the tolerance used for `all.equal()`
#'
#' @return Logical. TRUE if the two objects are roughly similar.
#' @noRd
.are_similar_ft <- function(x, y, tolerance = 10^-6) {
  # objects are assumed to have been pre-validated using .is_valid_ft()
  out_val <- FALSE
  attr(out_val, "reason") <- "critical error"
  # only the most important fields are compared, to save time
  fields_to_compare <- c(
    "dim_in", "dim_out", "blocksize", "cache_matrix",
    "fort_type", "inverse", "invertible", "fwd_par"
  )
  for (i in 1:length(fields_to_compare)) {
    cur_field <- fields_to_compare[i]
    if (!isTRUE(all.equal(x[[cur_field]], y[[cur_field]], tolerance = tolerance))) {
      attr(out_val, "reason") <- paste0(
        "discrepancy in field '",
        cur_field, "'"
      )
      return(out_val)
    }
  }
  # all tests passed: objects are similar
  out_val <- TRUE
  attr(out_val, "reason") <- "objects are similar"
  out_val
}

#' Generate a random permutation/expansion/contraction of the appropriate size
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
  available_methods <- .get_available_fort_types()
  if (is.character(fort_type)) {
    if (fort_type %in% names(available_methods)) {
      # if input is a known alias
      subclass_name <- available_methods[[fort_type]]
    } else {
      # possibly the name of a valid subclass
      subclass_name <- fort_type
    }
    # check if subclass exists and is valid
    if (exists(subclass_name)) {
      out_subclass <- get(subclass_name) # get subclass
      if (inherits(out_subclass, "R6ClassGenerator")) {
        # the object is actually a class generator
        # so, return its constructor method
        return(out_subclass$new)
      } else {
        # object is NOT a class constructor
        error_message <- paste0(
          "invalid `type` when calling fort(), since '",
          fort_type, "' is not a valid 'FastTransform'",
          " subclass; please choose one of: ",
          paste0(available_methods, collapse = ", ")
        )
      }
    } else {
      # object does not even exist
      error_message <- paste0(
        "invalid `type` when calling fort(), since '",
        fort_type, "' does not exist; please ",
        "choose one of: ",
        paste0(available_methods, collapse = ", ")
      )
    }
    stop(error_message)
  } else if (inherits(fort_type, "R6ClassGenerator")) {
    # assume it is a valid class generator for a
    # subclass that inherits from FastTransform, and
    # return its constructor
    return(fort_type$new)
  } else if (is.function(fort_type)) {
    # assume a valid constructor function is being
    # passed directly, so just return it
    return(fort_type)
  }
  error_message <- paste0(
    "invalid `type` when calling fort(); please ",
    "choose one of: ",
    paste0(available_methods, collapse = ", ")
  )
  stop(error_message)
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
