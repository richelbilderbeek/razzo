test_that("use", {
  expect_silent(check_mbd_paramses(create_test_mbd_paramses()))
})

test_that("values", {
  mbd_paramses <- create_test_mbd_paramses()
  expect_equal(length(mbd_paramses), 2)
  for (i in seq_along(mbd_paramses)) {
    mbd_params <- mbd_paramses[[i]]
    expect_equal(mbd_params$lambda,)
    expect_equal(mbd_params$mu,)
    expect_equal(mbd_params$nu,)
    expect_equal(mbd_params$q,)
    expect_equal(mbd_params$cond,)
    expect_equal(mbd_params$crown_age, get_razzo_crown_age())
    expect_equal(mbd_params$seed, i)
  }
})
