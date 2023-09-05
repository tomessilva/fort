#
# test - fort() helper function
#

test_that("first two arguments handled correctly (dim_in == dim_out)",{
  test_seed <- 1 * pi
  n_tests <- 2
  fort_types <- names(.get_available_fort_types())
  for (i in 1:length(fort_types)) {
    cur_type <- fort_types[i]
    for (j in 1:n_tests) {
      # set current seed
      cur_seed <- round(test_seed + i + j / n_tests, 4)
      # register current testing state
      cur_state <- paste0("/type:", cur_type, "/size:", j, "/seed:", cur_seed)
      # create three equivalent objects with current seed
      a1 <- fort(j, type = cur_type, seed = cur_seed)
      a2 <- fort(j, j, type = cur_type, seed = cur_seed)
      a3 <- fort(c(j, j), type = cur_type, seed = cur_seed)
      # check validity of objects
      cond <- .is_valid_ft(a1)
      expect_true(cond, paste0("a1 is not valid (reason:",
                               attr(cond, "reason"),cur_state,")"))
      cond <- .is_valid_ft(a2)
      expect_true(cond, paste0("a2 is not valid (reason:",
                               attr(cond, "reason"),cur_state,")"))
      cond <- .is_valid_ft(a3)
      expect_true(cond, paste0("a3 is not valid (reason:",
                               attr(cond, "reason"),cur_state,")"))
      # ensure objects are similar
      cond <- .are_similar_ft(a1, a2)
      expect_true(cond, paste0("a1 and a2 are not similar (reason:",
                               attr(cond, "reason"),cur_state,")"))
      cond <- .are_similar_ft(a1, a3)
      expect_true(cond, paste0("a1 and a3 are not similar (reason:",
                               attr(cond, "reason"),cur_state,")"))
      cond <- .are_similar_ft(a2, a3)
      expect_true(cond, paste0("a2 and a3 are not similar (reason:",
                               attr(cond, "reason"),cur_state,")"))
    }
  }
})

test_that("first two arguments handled correctly (dim_in < dim_out)",{
  test_seed <- 2 * pi
  n_tests <- 2
  fort_types <- names(.get_available_fort_types())
  for (i in 1:length(fort_types)) {
    cur_type <- fort_types[i]
    for (j in 1:n_tests) {
      # set current seed
      cur_seed <- round(test_seed + i + j / n_tests, 4)
      # register current testing state
      cur_state <- paste0("/type:", cur_type, "/size:", j, "/seed:", cur_seed)
      # create two equivalent objects with current seed
      a1 <- fort(1, j + 1, type = cur_type, seed = cur_seed)
      a2 <- fort(c(1, j + 1), type = cur_type, seed = cur_seed)
      # check validity of objects
      cond <- .is_valid_ft(a1)
      expect_true(cond, paste0("a1 is not valid (reason:",
                               attr(cond, "reason"),cur_state,")"))
      cond <- .is_valid_ft(a2)
      expect_true(cond, paste0("a2 is not valid (reason:",
                               attr(cond, "reason"),cur_state,")"))
      # ensure objects are similar
      cond <- .are_similar_ft(a1, a2)
      expect_true(cond, paste0("a1 and a2 are not similar (reason:",
                               attr(cond, "reason"),cur_state,")"))
    }
  }
})

test_that("first two arguments handled correctly (dim_in > dim_out)",{
  test_seed <- 3 * pi
  n_tests <- 2
  fort_types <- names(.get_available_fort_types())
  for (i in 1:length(fort_types)) {
    cur_type <- fort_types[i]
    for (j in 1:n_tests) {
      # set current seed
      cur_seed <- round(test_seed + i + j / n_tests, 4)
      # register current testing state
      cur_state <- paste0("/type:", cur_type, "/size:", j, "/seed:", cur_seed)
      # create two equivalent objects with current seed
      a1 <- fort(1, j + 1,  type = cur_type, seed = cur_seed)
      a2 <- fort(c(1, j + 1), type = cur_type, seed = cur_seed)
      # check validity of objects
      cond <- .is_valid_ft(a1)
      expect_true(cond, paste0("a1 is not valid (reason:",
                               attr(cond, "reason"),cur_state,")"))
      cond <- .is_valid_ft(a2)
      expect_true(cond, paste0("a2 is not valid (reason:",
                               attr(cond, "reason"),cur_state,")"))
      # ensure objects are similar
      cond <- .are_similar_ft(a1, a2)
      expect_true(cond, paste0("a1 and a2 are not similar (reason:",
                               attr(cond, "reason"),cur_state,")"))
    }
  }
})

test_that("'cache_matrix' argument is being respected",{
  cur_seed <- 4 * pi

  # test cache_matrix = FALSE
  a1 <- fort(16, 16, seed = cur_seed, cache_matrix = FALSE)
  expect_true(is.null(a1$fwd_mtrx), "a1's initial fwd_mtrx is not NULL")
  expect_true(is.null(a1$rev_mtrx), "a1's initial rev_mtrx is not NULL")
  irrelevant <- as.matrix(a1) # matrix should NOT be cached here
  expect_true(is.null(a1$fwd_mtrx), "a1 is storing fwd_mtrx (when it shouldn't)")
  a1_inv <- solve(a1) # get inverse transform
  irrelevant <- as.matrix(a1_inv) # matrix should NOT be cached here
  expect_true(is.null(a1$rev_mtrx), "a1_inv is storing rev_mtrx (when it shouldn't)")

  # test cache_matrix = TRUE
  a2 <- fort(16, 16, seed = cur_seed, cache_matrix = TRUE)
  expect_true(is.null(a2$fwd_mtrx), "a2's initial fwd_mtrx is not NULL")
  expect_true(is.null(a2$rev_mtrx), "a2's initial rev_mtrx is not NULL")
  irrelevant <- as.matrix(a2) # matrix SHOULD be cached here
  expect_false(is.null(a2$fwd_mtrx), "a2 is NOT storing fwd_mtrx (when it should)")
  expect_true(is.matrix(a2$fwd_mtrx), "a2 is NOT storing fwd_mtrx (when it should)")
  a2_inv <- solve(a2) # get inverse transform
  irrelevant <- as.matrix(a2_inv) # matrix SHOULD be cached here
  expect_false(is.null(a2$fwd_mtrx), "a2_inv is NOT storing rev_mtrx (when it should)")
  expect_true(is.matrix(a2$fwd_mtrx), "a2_inv is NOT storing rev_mtrx (when it should)")
})

test_that("different seeds result in different transforms",{
  test_seed <- 5 * pi
  n_tests <- 2
  fort_types <- names(.get_available_fort_types())
  for (i in 1:length(fort_types)) {
    cur_type <- fort_types[i]
    for (j in 1:n_tests) {
      # set current seed
      cur_seed <- round(test_seed + i + j / n_tests, 4)
      # register current testing state
      cur_state <- paste0("/type:", cur_type, "/size:", j, "/seeds:", cur_seed, "+", cur_seed + sqrt(2))
      # create two objects with different seeds
      a1 <- fort(17, 5 + j, type = cur_type, seed = cur_seed)
      a2 <- fort(17, 5 + j, type = cur_type, seed = cur_seed + sqrt(2))
      # check validity of objects
      cond <- .is_valid_ft(a1)
      expect_true(cond, paste0("a1 is not valid (reason:",
                               attr(cond, "reason"),cur_state,")"))
      cond <- .is_valid_ft(a2)
      expect_true(cond, paste0("a2 is not valid (reason:",
                               attr(cond, "reason"),cur_state,")"))
      # ensure objects are NOT similar
      cond <- .are_similar_ft(a1, a2)
      expect_false(cond, paste0("a1 and a2 are similar, even though they were",
                                "generated with different seeds (reason:",
                                attr(cond, "reason"),cur_state,")"))
    }
  }
})

test_that("error handling is working correctly",{
  cur_seed <- 6 * pi
  # invalid dimensions
  expect_error(fort(0, seed = cur_seed),
               "dim_in and dim_out must be positive when calling fort()")
  # invalid type of input for first argument
  cur_seed <- cur_seed + 1
  expect_error(fort(fort(1, seed = cur_seed), seed = cur_seed))
  cur_seed <- cur_seed + 1
  expect_error(fort(list(a=1), seed = cur_seed))
  # invalid type of input for second argument

  # invalid value for 'type' field
  cur_seed <- cur_seed + 1
  expect_error(fort(64, type = "invalid input", seed = cur_seed))
  cur_seed <- cur_seed + 1
  expect_error(fort(33, 22, type = list(), seed = cur_seed))

  # invalid value for the 'cache_matrix' field
  cur_seed <- cur_seed + 1
  expect_error(fort(11, 5, cache_matrix = "invalid input", seed = cur_seed))
  cur_seed <- cur_seed + 1
  expect_error(fort(4, 10, cache_matrix = list(), seed = cur_seed))
})
