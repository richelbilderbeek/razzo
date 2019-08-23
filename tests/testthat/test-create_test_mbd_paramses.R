test_that("use", {
  expect_silent(check_mbd_paramses(create_test_mbd_paramses()))
})

test_that("values", {
  mbd_paramses <- create_test_mbd_paramses()
  expect_equal(length(mbd_paramses), 2)
  for (i in seq_along(mbd_paramses)) {
    mbd_params <- mbd_paramses[[i]]
    expect_equal(mbd_params$lambda, 0.2)
    expect_equal(mbd_params$mu, 0.15)
    expect_equal(mbd_params$nu, 1.0)
    expect_equal(mbd_params$q, 0.1)
    expect_equal(mbd_params$cond, 1)
    expect_equal(mbd_params$crown_age, get_razzo_crown_age())
    expect_equal(mbd_params$seed, i)
  }
})
