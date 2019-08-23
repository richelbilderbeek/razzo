test_that("use", {
  razzo_paramses <- create_razzo_paramses(
    project_folder_name = peregrine::get_pff_tempdir()
  )
  for (razzo_params in razzo_paramses) {
    check_razzo_params(razzo_params)
  }
})

test_that("matches article", {
  # Issue 242, Isssue #242
  #
  # The mbd_params are checked elsewhere
  # The pir_params are checked elsewhere
  # The misc_params are irrelevant to be checked :-)
  #
  # Search for 'matches article' to find these
  #
  # These are the tests that looks for bigger picture issues and
  # relations between the parameter sets
  n_replicates <- 1
  mbd_paramses <- create_mbd_paramses(
    n_replicates = n_replicates
  )
  n_mbd_params <- length(mbd_paramses)

  razzo_paramses <- create_razzo_paramses(
    project_folder_name = peregrine::get_pff_tempdir(),
    mbd_paramses = mbd_paramses
  )
  n_razzo_paramses <- length(razzo_paramses)
  expect_equal(n_mbd_params, n_razzo_paramses)

  # RNG
  # experiment$beast2_options$rng_seed must match MBD RNG seed
  first_razzo_params <- razzo_paramses[[1]]
  expect_equal(
    first_razzo_params$mbd_params$seed,
    first_razzo_params$pir_params$experiments[[1]]$beast2_options$rng_seed,
  )
  expect_equal(
    first_razzo_params$mbd_params$seed,
    first_razzo_params$pir_params$experiments[[2]]$beast2_options$rng_seed,
  )
  expect_equal(
    first_razzo_params$mbd_params$seed,
    first_razzo_params$pir_params$alignment_params$rng_seed
  )
})
