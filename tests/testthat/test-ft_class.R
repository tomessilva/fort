test_that(".is_valid_ft() works correctly", {
  tmp_obj <- fort(1) # create normal FastTransform object
  expect_true(.is_valid_ft(tmp_obj),
              "fort object is not being recognized as valid")
  expect_true(.is_valid_ft(tmp_obj, quick = TRUE),
              "fort object is not being recognized as valid (quick = TRUE)")
  tmp_obj_2 <- list(a="1",b=1) # create object that is not FastTransform
  expect_false(.is_valid_ft(tmp_obj_2),
              "list is being recognized as valid")
  expect_false(.is_valid_ft(tmp_obj_2, quick = TRUE),
              "list is being recognized as valid (quick = TRUE)")
  tmp_obj_3 <- FastTransform$new() # create invalid object
  expect_false(.is_valid_ft(tmp_obj_3),
               "invalid FastTransform is being recognized as valid")
  expect_false(.is_valid_ft(tmp_obj_3, quick = TRUE),
               "invalid FastTransform is being recognized as valid (quick = TRUE)")
  tmp_obj_4 <- list(blocksize=2) # create fake FastTransform object
  class(tmp_obj_4) <- "FastTransform"
  expect_false(.is_valid_ft(tmp_obj_4, quick = FALSE),
               "fake FastTransform is being recognized as valid")
})

test_that("FastTransform$evaluate() works correctly", {
  n_tests <- 4
  tol_ <- 10^-8
  sizes_ <- ceiling(pi * (1:n_tests))
  for (i in sizes_) {
    tmp_obj <- fort(i)  # create fort object
    tmp_data <- diag(i) # create test matrix
    tmp_result_1 <- tmp_obj %*% tmp_data   # usual evaluation
    tmp_result_2 <- tmp_obj %***% tmp_data # unsafe evaluation
    tmp_result_3 <- tmp_obj$evaluate(tmp_data) # call evaluate()
    tmp_result_4 <- (tmp_data |> tmp_obj$evaluate()) # use pipe to evaluate
    expect_true(all.equal(tmp_result_1, tmp_result_3, tolerance = tol_),
                paste0("evaluate() call does not match usual evaluation",
                       " (size ", i, ")"))
    expect_true(all.equal(tmp_result_2, tmp_result_3, tolerance = tol_),
                paste0("evaluate() call does not match unsafe evaluation",
                       " (size ", i, ")"))
    expect_true(all.equal(tmp_result_1, tmp_result_4, tolerance = tol_),
                paste0("evaluate() call with pipe does not match usual",
                       " evaluation (size ", i, ")"))
    expect_true(all.equal(tmp_result_2, tmp_result_4, tolerance = tol_),
                paste0("evaluate() call with pipe does not match unsafe",
                       " evaluation (size ", i, ")"))
    expect_true(all.equal(tmp_result_3, tmp_result_4, tolerance = tol_),
                paste0("evaluate() calls with and without pipe do not match",
                       " (size ", i, ")"))
  }
})

test_that("FastTransform$get_dim(), get_nrow() and get_ncol() work correctly", {
  n_tests <- 4
  tol_ <- 10^-8
  sizes_ <- ceiling(sqrt(pi) * (1:n_tests))
  for (i in sizes_) {
    m <- fort(i, i)
    expect_true(all.equal(m$get_nrow(), i, tolerance = tol_),
                "get_nrow() is giving wrong values")
    expect_true(all.equal(m$get_ncol(), i, tolerance = tol_),
                "get_ncol() is giving wrong values")
    expect_true(all.equal(m$get_dim()[1], m$get_nrow(), tolerance = tol_),
                "get_dim()[1] and get_nrow() do not match")
    expect_true(all.equal(m$get_dim()[2], m$get_ncol(), tolerance = tol_),
                "get_dim()[2] and get_ncol() do not match")
    m <- fort(1, i)
    expect_true(all.equal(m$get_nrow(), i, tolerance = tol_),
                "get_nrow() is giving wrong values")
    expect_true(all.equal(m$get_ncol(), 1, tolerance = tol_),
                "get_ncol() is giving wrong values")
    expect_true(all.equal(m$get_dim()[1], m$get_nrow(), tolerance = tol_),
                "get_dim()[1] and get_nrow() do not match")
    expect_true(all.equal(m$get_dim()[2], m$get_ncol(), tolerance = tol_),
                "get_dim()[2] and get_ncol() do not match")
    m <- fort(i, 2)
    expect_true(all.equal(m$get_nrow(), 2, tolerance = tol_),
                "get_nrow() is giving wrong values")
    expect_true(all.equal(m$get_ncol(), i, tolerance = tol_),
                "get_ncol() is giving wrong values")
    expect_true(all.equal(m$get_dim()[1], m$get_nrow(), tolerance = tol_),
                "get_dim()[1] and get_nrow() do not match")
    expect_true(all.equal(m$get_dim()[2], m$get_ncol(), tolerance = tol_),
                "get_dim()[2] and get_ncol() do not match")
    # test under inverse
    m <- m$get_inverse()
    expect_true(all.equal(m$get_nrow(), i, tolerance = tol_),
                "get_nrow() is giving wrong values")
    expect_true(all.equal(m$get_ncol(), 2, tolerance = tol_),
                "get_ncol() is giving wrong values")
    expect_true(all.equal(m$get_dim()[1], m$get_nrow(), tolerance = tol_),
                "get_dim()[1] and get_nrow() do not match")
    expect_true(all.equal(m$get_dim()[2], m$get_ncol(), tolerance = tol_),
                "get_dim()[2] and get_ncol() do not match")

  }
})

test_that("FastTransform$get_inverse() works correctly", {
  tol_ <- 10^-8
  n_tests <- 8
  for (i in 2:(n_tests+1)) {
    tmp_obj <- fort(i, i)
    tmp_obj_inv <- tmp_obj$get_inverse()
    expect_true(.are_similar_ft(tmp_obj, tmp_obj_inv$get_inverse(), tolerance = tol_),
                "double inversion does not recover original object")
  }
  # test handling of invalid objects
  tmp_obj <- fort(2)
  tmp_obj$blocksize <- NULL
  expect_error(tmp_obj$get_inverse())
})

test_that("FastTransform$get_transpose() works correctly", {
  tol_ <- 10^-8
  n_tests <- 8
  for (i in (2^(1:n_tests))) {
    tmp_obj <- fort(i, i)
    tmp_obj_inv <- tmp_obj$get_transpose()
    expect_true(.are_similar_ft(tmp_obj, tmp_obj_inv$get_transpose(), tolerance = tol_),
                "double transposition does not recover original object")
  }
})

test_that("FastTransform$get_logdet() works correctly", {
  tol_ <- 10^-8
  n_tests <- 8
  for (i in 2:(n_tests+1)) {
    tmp_obj <- fort(i, i)
    tmp_obj_logdet <- tmp_obj$get_logdet()
    expect_true(is.list(tmp_obj_logdet),
                "output object is not a list")
    expect_true("det" %in% class(tmp_obj_logdet),
                "output object is not of class 'det'")
    expect_true("modulus" %in% names(tmp_obj_logdet),
                "output object does not contain field 'modulus'")
    expect_true("sign" %in% names(tmp_obj_logdet),
                "output object does not contain field 'sign'")
    expect_true(attr(tmp_obj_logdet$modulus,"logarithm"),
                "output object is det and NOT logdet")
    # check inverse and cache handling
    tmp_obj_logdet2 <- tmp_obj$get_logdet()
    expect_true(is.list(tmp_obj_logdet2))
    tmp_obj_inv <- solve(tmp_obj)
    tmp_obj_logdet3 <- tmp_obj_inv$get_logdet()
    expect_true(is.list(tmp_obj_logdet3))
    tmp_obj_logdet4 <- tmp_obj_inv$get_logdet()
    expect_true(is.list(tmp_obj_logdet4))
  }
})

test_that("FastTransform$get_norm() works correctly", {
  types_to_test <- c("o", "i", "f", "m", "1", "2")
  n_tests <- 4
  for (i in (2^(1:n_tests))) {
    tmp_obj <- fort(i, i)
    for (cur_type in types_to_test) {
      tmp_obj_norm <- tmp_obj$get_norm(type = cur_type)
      expect_true(is.numeric(tmp_obj_norm),
                  "returned value is not numeric")
      expect_true(length(tmp_obj_norm) == 1,
                  "returned value is not of length 1")
    }
  }
  # test error handling
  expect_error(tmp_obj_norm <- tmp_obj$get_norm(type = "x"))
})

test_that("FastTransform$get_norm_margin() works correctly", {
  types_to_test <- c("o", "i", "f", "m", "1", "2")
  n_tests <- 2
  for (i in (4^(1:n_tests))) {
    tmp_obj <- fort(i, 1) # nrow = 1, ncol = i
    for (cur_type in types_to_test) {
      tmp_obj_norm_row <- tmp_obj$get_norm_margin(type = cur_type, by = 1)
      expect_true(is.numeric(tmp_obj_norm_row),
                  "returned row norm is not numeric")
      expect_true(length(tmp_obj_norm_row) == 1,
                  "returned row norm is not of length 1")

      tmp_obj_norm_col <- tmp_obj$get_norm_margin(type = cur_type, by = 2)
      expect_true(is.numeric(tmp_obj_norm_col),
                  "returned col norm is not numeric")
      expect_true(length(tmp_obj_norm_col) == i,
                  paste0("returned col norm is not of length ", i))

    }
  }
  # test error handling
  expect_error(tmp_obj_norm <- tmp_obj$get_norm_margin(type = "x"))
})

test_that("FastTransform$as_matrix() works correctly", {
  n_tests <- 2
  tol_ <- 10^-8
  for (i in (2^(1:n_tests))) {
    tmp_obj <- fort(i, 1)
    tmp_mtrx <- tmp_obj$as_matrix()
    expect_true(is.matrix(tmp_mtrx),
                "output object is not of matrix type")
    expect_true(all.equal(dim(tmp_obj),dim(tmp_mtrx),tolerance = tol_),
                "output object dimensions are not correct")
    tmp_obj <- fort(i, 1)
    tmp_obj_inv <- solve(tmp_obj)
    tmp_matrix_inv <- tmp_obj_inv$as_matrix()
    expect_true(is.matrix(tmp_matrix_inv),
                "output object is not of matrix type")
    expect_true(all.equal(dim(tmp_obj_inv),dim(tmp_matrix_inv),tolerance = tol_),
                "output object dimensions are not correct")

  }
  # test error handling
  tmp_obj$blocksize <- NULL
  expect_error(tmp_obj$as_matrix())
})

test_that("FastTransform$print() works correctly", {
  # redirect console output to a temporary file
  tmp_file <- tempfile()
  sink(tmp_file)
  on.exit({
    # restore console output
    sink()
    # remove temporary file, if it exists
    if (file.exists(tmp_file)) unlink(tmp_file)
  })
  tmp_obj <- fort(1)
  expect_true(.is_valid_ft(tmp_obj$print()),
              "self is not being returned invisibly")
  tmp_obj <- solve(fort(2,3))
  expect_true(.is_valid_ft(tmp_obj$print()),
              "self is not being returned invisibly")
  # check error handling
  tmp_obj$blocksize <- NULL # make invalid object
  expect_error(tmp_obj$print())
})

test_that("FastTransform$summary() works correctly", {
  # redirect console output to a temporary file
  tmp_file <- tempfile()
  sink(tmp_file)
  on.exit({
    # restore console output
    sink()
    # remove temporary file, if it exists
    if (file.exists(tmp_file)) unlink(tmp_file)
  })
  tmp_obj <- fort(1)
  expect_true(.is_valid_ft(tmp_obj$summary()),
              "self is not being returned invisibly")
  tmp_obj <- solve(fort(2,3))
  expect_true(.is_valid_ft(tmp_obj$summary()),
              "self is not being returned invisibly")
  # check error handling
  tmp_obj$blocksize <- NULL # make invalid object
  expect_error(tmp_obj$summary())
})

test_that("default FastTransform placeholder functions return NULL", {
  # create invalid FastTransform object
  tmp_ft <- FastTransform$new()
  # test placeholder functions
  expect_true(is.null(tmp_ft$fwd_eval(NULL)))
  expect_true(is.null(tmp_ft$rev_eval(NULL)))
  expect_true(is.null(tmp_ft$calculate_rev_par()))
})

test_that("FastTransform$evaluate() handles odd inputs correctly", {
  tmp_fort <- fort(2)
  tmp_data <- diag(2)
  # evaluate with vector input
  expect_true(is.matrix(tmp_fort %*% 1:2))
  expect_true(is.matrix(fort(1,16) %*% 1:8))
  # evaluate with input of wrong type
  expect_error(tmp_fort$evaluate("a"))
  # evaluate with non-conforming inputs
  expect_error(tmp_fort$evaluate(1:10))
  expect_error(tmp_fort$evaluate(matrix(1:12,nrow=3,ncol=4)))
  # make FastTransform invalid
  tmp_fort$blocksize <- NULL
  expect_error(tmp_fort$evaluate(tmp_data))
})

test_that("FastTransform$get_n_par() works correctly", {
  # test invertible
  tests <- c(2, 4, 8, 16)
  for (cur_size in tests) {
    tmp_fort <- fort(cur_size)
    tmp_par <- tmp_fort$get_n_par()
    expect_true(is.numeric(tmp_par))
    expect_equal(tmp_par, length(unlist(tmp_fort$fwd_par)))
  }
  # test non-invertible
  tests <- list(c(1, 2),
                c(2, 1),
                c(3, 16))
  for (cur_size in tests) {
    tmp_fort <- solve(fort(cur_size))
    tmp_par <- tmp_fort$get_n_par()
    expect_true(is.numeric(tmp_par))
    expect_equal(tmp_par, prod(cur_size))
  }
})

test_that("FastTransform$get_transpose() works for non-invertible objects", {
  tmp_fort <- fort(3,5)
  tmp_matrix <- suppressWarnings(tmp_fort$get_transpose())
  expect_true(is.matrix(tmp_matrix))
})
