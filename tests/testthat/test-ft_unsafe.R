test_that("the %***%.FastTransform operator works correctly", {
  tests <- c(2,3,4,16)
  tol_ <- 10^-8
  for (cur_size in tests) {
    tmp_fort <- fort(cur_size)
    tmp_data <- diag(cur_size)
    result1 <- tmp_fort %***% tmp_data
    result2 <- tmp_fort %*% tmp_data
    result3 <- tmp_data |> tmp_fort$evaluate()
    expect_equal(result1,result2,tolerance=tol_)
    expect_equal(result1,result3,tolerance=tol_)
  }
})

test_that("the %***%.FastTransform operator works for inverse transforms", {
  tests <- list(c(2,2),
                c(2,3),
                c(3,2),
                c(8,8))
  tol_ <- 10^-8
  for (cur_size in tests) {
    tmp_fort <- fort(cur_size)
    tmp_data <- diag(cur_size[2])
    tmp_fort_inv <- solve(tmp_fort)
    tmp_result1 <- tmp_fort_inv %***% tmp_data
    tmp_result2 <- tmp_fort_inv %*% tmp_data
    expect_equal(tmp_result1,tmp_result2,tolerance=tol_)
  }
})
