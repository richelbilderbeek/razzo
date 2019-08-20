test_that("use", {

  mbd_params <- create_test_mbd_params()
  pir_params <- peregrine::create_test_pff_pir_params(
    twinning_params = peregrine::create_pff_twinning_params()
  )
  misc_params <- create_misc_params()

  expect_silent(
    create_params_razzo(
      mbd_params = mbd_params,
      pir_params = pir_params,
      misc_params = misc_params
    )
  )
})

