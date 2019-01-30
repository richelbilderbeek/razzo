context("test-create_mbd_params")

test_that("use", {

  mbd_params <- create_mbd_params(
    lambda = 0.1,
    mu = 0.15,
    nu = 0.2,
    q = 0.01,
    crown_age = 15.0,
    cond = 1,
    seed = 1
  )
  expect_true(
    mbd_params$lambda >= 0
  )
  expect_true(
    mbd_params$mu >= 0
  )
  expect_true(
    mbd_params$nu >= 0
  )
  expect_true(
    mbd_params$q >= 0
  )
  expect_true(
    mbd_params$q <= 1
  )
  expect_true(
    mbd_params$crown_age > 0
  )
  expect_true(
    mbd_params$cond >= 0
  )
  expect_true(
    is.numeric(mbd_params$seed)
  )
})
