context("test-check_razzo_params")

test_that("use", {

  good_razzo_params <- create_test_razzo_params()
  expect_silent(
    check_razzo_params(razzo_params = good_razzo_params)
  )

  mbd_params <- create_test_mbd_params()
  pir_params <- peregrine::create_test_pff_pir_params(
    twinning_params = peregrine::create_pff_twinning_params()
  )
  misc_params <- create_misc_params()

  expect_silent(
    create_razzo_params(
      mbd_params = mbd_params,
      pir_params = pir_params,
      misc_params = misc_params
    )
  )

  # Check elements
  expect_error(
    check_razzo_params(razzo_params = list()),
    "'mbd_params' must be an element of a 'razzo_params'"
  )

  expect_error(
    check_razzo_params(razzo_params = list(mbd_params = mbd_params)),
    "'pir_params' must be an element of a 'razzo_params'"
  )

  expect_error(
    check_razzo_params(razzo_params = list(
      mbd_params = mbd_params, pir_params = pir_params)
    ),
    "'misc_params' must be an element of a 'razzo_params'"
  )

  # Check mbd_params
  # done by check_mbd_params

  # Check pir_params
  # Done by pirouette::check_pir_params
  # and peregrine::check_pff_pir_params

  # Check misc_params
  razzo_params <- good_razzo_params
  razzo_params$misc_params$tree_filename <- "/tmp/puf"
  expect_error(
    check_razzo_params(razzo_params),
    "Peregrine-unfriendly filename for '"
  )
})
