test_that("the %*%.FastTransform operator works correctly", {
  test_sizes <- c(2,7,16)
  tol_ <- 10^-8
  for (cur_size in test_sizes) {
    tmp_transform <- fort(cur_size)
    tmp_matrix <- diag(cur_size)
    result1 <- tmp_transform %*% tmp_matrix
    result2 <- tmp_transform$evaluate(tmp_matrix)
    expect_equal(result1,result2,tolerance=tol_)
  }
})

test_that("dim.FastTranform() works correctly", {
  tests <- list(c(1,2),
                c(3,2),
                c(16,7),
                c(5,5),
                c(16,16))
  for (i in 1:length(tests)) {
    cur_vec <- tests[[i]]
    tmp_transform <- fort(cur_vec)
    test_vec <- rev(dim(tmp_transform))
    expect_equal(cur_vec,test_vec)
  }
})

test_that("as.matrix.FastTransform() works correctly", {
  test_sizes <- c(2,7,16)
  tol_ <- 10^-8
  for (cur_size in test_sizes) {
    tmp_transform <- fort(cur_size)
    result1 <- as.matrix(tmp_transform)
    result2 <- tmp_transform$as_matrix()
    expect_true(is.matrix(result1))
    expect_true(is.matrix(result2))
    expect_equal(result1,result2,tolerance=tol_)
  }
})

test_that("summary.FastTransform() works correctly", {
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
  expect_true(.is_valid_ft(summary(tmp_obj)))
})

test_that("determinant.FastTransform() works correctly", {
  expect_true(is.numeric(det(fort(2))))
  expect_error(is.numeric(det(fort(3,5))))
})

test_that("t.FastTransform() works correctly for invertible inputs", {
  tests <- c(2,4,16)
  for (cur_size in tests) {
    tmp_fort <- fort(cur_size)
    tmp_fort2 <- t(tmp_fort)
    tmp_fort3 <- t(tmp_fort2)
    expect_true(.are_similar_ft(tmp_fort,tmp_fort3))
  }
})

test_that("solve.FastTransform() works correctly", {
  tests <- c(2,4,5,16)
  tol_ <- 10^-8
  for (cur_size in tests) {
    tmp_fort <- fort(cur_size)
    tmp_data <- diag(cur_size)
    tmp_fort_inv <- solve(tmp_fort) # get pseudoinverse
    tmp_x_1 <- tmp_fort_inv %*% tmp_data
    tmp_x_2 <- solve(tmp_fort, tmp_data)
    expect_equal(tmp_x_1,tmp_x_2,tolerance=tol_)
    tmp_y_1 <- tmp_fort %*% tmp_x_1
    tmp_y_2 <- tmp_fort %*% tmp_x_2
    expect_equal(tmp_y_1,tmp_y_2,tolerance=tol_)
    expect_equal(tmp_data,tmp_y_2,tolerance=tol_)
    expect_equal(tmp_y_1,tmp_data,tolerance=tol_)
  }
})

test_that("determinant.FastTransform() works ok with logarithm=FALSE", {
  test_det <- determinant(fort(4),logarithm = FALSE)
  expect_true(is.list(test_det))
  expect_true(inherits(test_det,"det"))
  expect_false(attr(test_det$modulus,"logarithm"))
})

test_that("t() warns user when used on non-invertible transforms", {
  tmp_fort <- fort(3,4)
  suppressWarnings(expect_warning(t(tmp_fort)))
})
