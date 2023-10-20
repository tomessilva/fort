test_that("FastTransformFFT1$new() works", {
  tests <- list(c(1,1,2),
                c(1,2,2),
                c(2,1,2),
                c(3,4,4),
                c(4,4,4),
                c(4,4,16),
                c(13,25,32))
  for (cur_size in tests) {
    tmp_ft <- FastTransformFFT1$new(dim_in = cur_size[1],
                                    dim_out = cur_size[2],
                                    blocksize = cur_size[3])
    expect_true(.is_valid_ft(tmp_ft))
    expect_true(ncol(tmp_ft) == cur_size[1])
    expect_true(nrow(tmp_ft) == cur_size[2])
  }
})

test_that("FastTransformFFT1$fwd_eval() works", {
  tests <- list(c(1,7,8),
                c(1,2,2),
                c(2,7,8),
                c(3,4,4),
                c(4,4,4),
                c(4,4,16),
                c(13,25,32))
  for (cur_size in tests) {
    tmp_ft <- FastTransformFFT1$new(dim_in = cur_size[1],
                                    dim_out = cur_size[2],
                                    blocksize = cur_size[3])
    tmp_data <- matrix(rnorm(cur_size[1]*cur_size[3]),
                       ncol=cur_size[3],
                       nrow=cur_size[1])
    tmp_result <- NULL
    try(tmp_result <- tmp_ft$fwd_eval(tmp_data))
    expect_true(!is.null(tmp_result))
    expect_true(is.matrix(tmp_result))
    expect_true(ncol(tmp_result) == ncol(tmp_data))
    expect_true(nrow(tmp_result) == cur_size[2])
  }
})

test_that("FastTransformFFT1$calculate_rev_par() works", {
  tests <- list(c(1,1,2),
                c(1,2,2),
                c(2,1,2),
                c(3,4,4),
                c(4,4,4),
                c(4,4,16),
                c(13,25,32))
  for (cur_size in tests) {
    tmp_ft <- FastTransformFFT1$new(dim_in = cur_size[1],
                                    dim_out = cur_size[2],
                                    blocksize = cur_size[3])
    expect_true(is.null(tmp_ft$rev_par))
    tmp_ft <- tmp_ft$calculate_rev_par()
    expect_true(.is_valid_ft(tmp_ft))
    expect_true(is.list(tmp_ft$rev_par))
  }
})

test_that("FastTransformFFT1$rev_eval() works", {
  tests <- list(c(6,7,8),
                c(2,2,2),
                c(2,7,8),
                c(3,4,4),
                c(4,4,4),
                c(4,4,16),
                c(13,25,32))
  for (cur_size in tests) {
    tmp_ft <- FastTransformFFT1$new(dim_in = cur_size[1],
                                    dim_out = cur_size[2],
                                    blocksize = cur_size[3])
    tmp_ft <- tmp_ft$calculate_rev_par()
    tmp_data <- matrix(rnorm(cur_size[2]*cur_size[3]),
                       ncol=cur_size[3],
                       nrow=cur_size[2])
    tmp_result <- NULL
    try(tmp_result <- tmp_ft$rev_eval(tmp_data))
    expect_true(!is.null(tmp_result))
    expect_true(is.matrix(tmp_result))
    expect_true(ncol(tmp_result) == ncol(tmp_data))
    expect_true(nrow(tmp_result) == cur_size[1])
  }
})


