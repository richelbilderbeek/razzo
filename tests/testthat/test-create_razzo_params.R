context("create_razzo_params")

test_that("use", {

  mbd_params <- becosys::create_mbd_params(
    lambda = 0.1,
    mu = 0.15,
    nu = 0.2,
    q = 0.01
  )
  pir_params <- pirouette::create_pir_params(
    alignment_params = create_test_alignment_params()
  )
  razzo_params <- create_razzo_params(
    mbd_params = mbd_params,
    pir_params = pir_params
  )

  expect_equal(mbd_params, razzo_params$mbd_params)
  expect_equal(pir_params, razzo_params$pir_params)
})
