context("test-check_razzo_params")

test_that("use", {

  good_razzo_params <- create_test_razzo_params()
  expect_silent(
    check_razzo_params(razzo_params = good_razzo_params)
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

  # Check for Peregrine-unfriendly filenames (hence 'puf')
  razzo_params <- good_razzo_params
  razzo_params$pir_params$twinning_params$twin_tree_filename <- "/tmp/puf"
  expect_error(
    check_razzo_params(razzo_params),
    "Peregrine-unfriendly filename for '"
  )

  razzo_params <- good_razzo_params
  razzo_params$pir_params$twinning_params$twin_alignment_filename <- "/tmp/puf"
  expect_error(
    check_razzo_params(razzo_params),
    "Peregrine-unfriendly filename for '"
  )

  razzo_params <- good_razzo_params
  razzo_params$pir_params$twinning_params$twin_evidence_filename <- "/tmp/puf"
  expect_error(
    check_razzo_params(razzo_params),
    "Peregrine-unfriendly filename for '"
  )

  razzo_params <- good_razzo_params
  razzo_params$pir_params$alignment_params$fasta_filename <- "/tmp/puf"
  expect_error(
    check_razzo_params(razzo_params),
    "Peregrine-unfriendly filename for '"
  )

  razzo_params <- good_razzo_params
  razzo_params$pir_params$evidence_filename <- "/tmp/puf.csv"
  expect_error(
    check_razzo_params(razzo_params),
    "Peregrine-unfriendly filename for '"
  )

  for (i in seq_along(pir_params$experiments)) {
    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$beast2_options$input_filename <- "/tmp/puf"
    expect_error(
      check_razzo_params(razzo_params),
      "Peregrine-unfriendly filename for '"
    )

    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$beast2_options$output_log_filename <- "/tmp/puf"
    expect_error(
      check_razzo_params(razzo_params),
      "Peregrine-unfriendly filename for '"
    )

    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$beast2_options$output_trees_filenames <- "/tmp/puf"
    expect_error(
      check_razzo_params(razzo_params),
      "Peregrine-unfriendly filename for '"
    )

    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$beast2_options$output_state_filename <- "/tmp/puf"
    expect_error(
      check_razzo_params(razzo_params),
      "Peregrine-unfriendly filename for '"
    )

    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$beast2_options$beast2_working_dir <- "/tmp/puf"
    expect_error(
      check_razzo_params(razzo_params),
      "Peregrine-unfriendly filename for '"
    )

    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$beast2_options$beast2_path <- "/tmp/puf"
    expect_error(
      check_razzo_params(razzo_params),
      "Peregrine-unfriendly filename for '"
    )

    razzo_params <- good_razzo_params
    razzo_params$pir_params$experiments[[i]]$errors_filename <- "/tmp/puf.csv"
    expect_error(
      check_razzo_params(razzo_params),
      "Peregrine-unfriendly filename for '"
    )
  }

  # Check misc_params
  razzo_params <- good_razzo_params
  razzo_params$misc_params$tree_filename <- "/tmp/puf"
  expect_error(
    check_razzo_params(razzo_params),
    "Peregrine-unfriendly filename for '"
  )
})
