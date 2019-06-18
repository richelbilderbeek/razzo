context("test-create_razzo_params")

test_that("use", {

  mbd_params <- create_test_mbd_params()
  pir_params <- create_test_pff_pir_params()
  misc_params <- create_misc_params()

  razzo_params <- create_razzo_params(
    mbd_params = mbd_params,
    pir_params = pir_params,
    misc_params = misc_params
  )

  expect_equal(mbd_params, razzo_params$mbd_params)
  expect_equal(pir_params, razzo_params$pir_params)
  expect_equal(misc_params, razzo_params$misc_params)
})
