test_that("use", {

  mbd_params <- create_test_mbd_params()
  pir_params <- razzo::create_test_razzo_pir_params()
  misc_params <- create_misc_params()

  razzo_params <- create_params_razzo(
    mbd_params = mbd_params,
    pir_params = pir_params,
    misc_params = misc_params
  )

  expect_silent(check_razzo_params(razzo_params))

  # create_params_razzo changes the tree filename to follow
  # the alignment filename in pir_params
  expect_true(
    misc_params$tree_filename != razzo_params$misc_params$tree_filename
  )
})
