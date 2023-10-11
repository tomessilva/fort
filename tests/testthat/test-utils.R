#
# test - utility functions
#

test_that(".get_available_fort_types() returns a list with valid options", {
  list_of_types <- .get_available_fort_types()
  expect_true(is.list(list_of_types), "returned object is not a list")
  expect_true("default" %in% names(list_of_types), "'default' fort type not defined")
  for (i in 1:length(list_of_types)) {
    cur_type_gen <- get0(list_of_types[[i]])
    # ensure cur_type_gen is a generator
    cur_condition <- FALSE
    if (!is.null(cur_type_gen)) {
      if ("R6ClassGenerator" %in% class(cur_type_gen)) {
        cur_condition <- TRUE
      }
    }
    expect_true(cur_condition, paste0("'", list_of_types[[i]],
                                      "' is not a valid",
                                      " R6ClassGenerator object"))
    # verify that the generated objects are valid
    tmp_object <- NULL
    try(tmp_object <- cur_type_gen$new(dim_in = 2, dim_out = 2, blocksize = 2))
    cur_condition <- FALSE
    if (!is.null(tmp_object)) {
      if ("FastTransform" %in% class(tmp_object)) {
        if (.is_valid_ft(tmp_object)) {
          cur_condition <- TRUE
        }
      }
    }
    expect_true(cur_condition, paste0("'", list_of_types[[i]],
                                      "$new()' does not generate valid",
                                      " FastTransform objects"))
  }
})

test_that(".are_similar_ft() is working correctly", {
  cur_seed <- 1
  tmp_object_1 <- fort(dim_in = 2, dim_out = 2, seed = cur_seed)
  tmp_object_2 <- fort(dim_in = 2, dim_out = 2, seed = cur_seed)
  tmp_object_3 <- fort(dim_in = 2, dim_out = 2, seed = cur_seed + 1)
  expect_true(.are_similar_ft(tmp_object_1, tmp_object_2),
              "objects with same seed are NOT being considered similar")
  expect_true(!.are_similar_ft(tmp_object_1, tmp_object_3),
              "objects with different seed are being considered similar")
  expect_true(!.are_similar_ft(tmp_object_2, tmp_object_3),
              "objects with different seed are being considered similar")
})

test_that(".get_dims_from_inputs() is working correctly", {
  n_tests <- 16
  for (i in 1:n_tests) {
    tmp_obj <- .get_dims_from_inputs(dim_in = i, dim_out = i)
    target_blocksize <- max(2,2^ceiling(log(i)/log(2)))
    expect_true(is.list(tmp_obj), "output is not a list")
    expect_true(all.equal(tmp_obj$dim_in, i), "dim_in does not match input")
    expect_true(all.equal(tmp_obj$dim_out, i), "dim_out does not match input")
    expect_true(all.equal(tmp_obj$blocksize, target_blocksize),
                "blocksize does not match expected value")
  }
})

test_that(".get_fort_constructor() is working correctly", {
  list_of_types <- .get_available_fort_types()
  for (i in 1:length(list_of_types)) {
    cur_string <- names(list_of_types)[i]
    cur_classgen <- get0(list_of_types[[i]])
    fort_const <- list()
    # test string input
    fort_const[[1]] <- .get_fort_constructor(fort_type = cur_string)
    # test R6ClassGenerator input
    fort_const[[2]] <- .get_fort_constructor(fort_type = cur_classgen)
    # test constructor input
    fort_const[[3]] <- .get_fort_constructor(fort_type = cur_classgen$new)
    for (j in 1:length(fort_const)) {
      tmp_fort <- fort_const[[j]](dim_in = 2, dim_out = 2, blocksize = 2)
      expect_true(.is_valid_ft(tmp_fort),
                  paste0("invalid constructor when testing option '",
                         cur_string,"' on iter ",j))
    }
  }
})

test_that(".get_random_permutation() is working correctly", {
  n_tests <- 16
  for (i in 1:n_tests) {
    tmp_obj_1 <- .get_random_permutation(dim_in = 1, dim_out = i)
    tmp_obj_2 <- .get_random_permutation(dim_in = i, dim_out = 1)
    tmp_obj_3 <- .get_random_permutation(dim_in = i, dim_out = i)
    expect_true(length(tmp_obj_1) == i, "wrong permutation length")
    expect_true(length(tmp_obj_2) == 1, "wrong permutation length")
    expect_true(length(tmp_obj_3) == i, "wrong permutation length")
  }
})

test_that(".get_printing_symbols() is working correctly", {
  tmp_obj_1 <- .get_printing_symbols()
  tmp_obj_2 <- .get_printing_symbols(simple = FALSE)
  tmp_obj_3 <- .get_printing_symbols(simple = TRUE)
  expect_true(is.list(tmp_obj_1), "does not return a list (without input)")
  expect_true(is.list(tmp_obj_2), "does not return a list when input=FALSE")
  expect_true(is.list(tmp_obj_3), "does not return a list when input=TRUE")
})

test_that(".pseudoinvert_scaling() is working correctly", {
  n_tests <- 16
  tol_ <- 10^-8
  for (i in 1:n_tests) {
    random_vector <- 1 + runif(i)
    random_vector_2 <- .pseudoinvert_scaling(.pseudoinvert_scaling(random_vector))
    expect_true(all.equal(random_vector, random_vector_2, tolerance = tol_),
                paste0("failed inverting scaling vector of size ", i))
  }
})

test_that(".pseudoinvert_permutation() is working correctly", {
  n_tests <- 16
  tol_ <- 10^-8
  for (i in 1:n_tests) {
    random_perm_1 <- .get_random_permutation(dim_in = i, dim_out = i)
    random_perm_2 <- .pseudoinvert_permutation(random_perm_1)
    random_perm_3 <- .pseudoinvert_permutation(random_perm_2)
    expect_true(all.equal(random_perm_1, random_perm_3, tolerance = tol_),
                paste0("failed inverting permutation of size ", i))
  }
})
