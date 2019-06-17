context("test-check_razzo_params")

test_that("use", {
  expect_silent(
    check_razzo_params(razzo_params = create_test_razzo_params())
  )

  mbd_params <- create_test_mbd_params()
  pir_params <- create_test_pff_pir_params()
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
  # Mostly done by check_pir_params
  expect_true(is_pff(pir_params$twinning_params$twin_tree_filename))
  expect_true(is_pff(pir_params$twinning_params$twin_alignment_filename))
  expect_true(is_pff(pir_params$twinning_params$twin_evidence_filename))
  expect_true(is_pff(pir_params$alignment_params$fasta_filename))
  for (experiment in pir_params$experiments) {
    expect_true(is_pff(experiment$beast2_options$input_filename))
    expect_true(is_pff(experiment$beast2_options$output_log_filename))
    expect_true(is_pff(experiment$beast2_options$output_trees_filenames))
    expect_true(is_pff(experiment$beast2_options$output_state_filename))
    expect_true(is_pff(experiment$beast2_options$beast2_working_dir))
    expect_true(is_pff(experiment$beast2_options$beast2_path))
  }
  # Check misc_params
  expect_true(is_pff(misc_params$tree_filename))
})
