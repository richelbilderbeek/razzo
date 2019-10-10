test_that("use", {
  mbd_paramses <- create_mbd_paramses()
  for (mbd_params in mbd_paramses) {
    expect_silent(check_mbd_params(mbd_params))
  }
})

test_that("equal values as table", {
  df <- create_mbd_params_table()
  mbd_paramses <- create_mbd_paramses()
  expect_equal(nrow(df), length(mbd_paramses))
  for (i in seq_along(mbd_paramses)) {
    mbd_params <- mbd_paramses[[i]]
    expect_equal(df$lambda[i], mbd_params$lambda)
    expect_equal(df$mu[i], mbd_params$mu)
    expect_equal(df$nu[i], mbd_params$nu)
    expect_equal(df$q[i], mbd_params$q)
    expect_equal(df$crown_age[i], mbd_params$crown_age)
    expect_equal(df$cond[i], mbd_params$cond)
    expect_equal(df$seed[i], mbd_params$seed)
  }
})

test_that("can plug in mbd.TaxaAndMbness::create_params_table", {

  mbd_paramses <- create_mbd_paramses()
  for (mbd_params in mbd_paramses) {
    expect_silent(check_mbd_params(mbd_params))
  }

  mbd_paramses <- mbd.TaxaAndMbness::create_params_table()
  for (mbd_params in mbd_paramses) {
    expect_silent(check_mbd_params(mbd_params))
  }


})
