test_that("use", {
  if (!beastier::is_on_travis()) return()
  razzo_paramses <- create_razzo_paramses(
    project_folder_name = peregrine::get_pff_tempdir()
  )
  for (razzo_params in razzo_paramses) {
    check_razzo_params(razzo_params)
  }
})

test_that("all filenames are Peregrine friendly", {

  if (!beastier::is_on_travis()) return()

  razzo_paramses <- create_razzo_paramses(
    project_folder_name = peregrine::get_pff_tempdir()
  )

  flat_params_set <- unlist(razzo_paramses)
  names <- names(flat_params_set)
  filename_indices <- which(
    grepl(pattern = "(filename|working_dir)", x = names)
  )
  filenames <- flat_params_set[filename_indices]
  for (filename in filenames) {
    expect_true(
      filename == "" ||
      beautier::is_one_na(filename) ||
      peregrine::is_pff(filename)
    )
  }
})


test_that("matches article", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

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
  expect_equal(
    first_razzo_params$pir_params$experiments[[1]]$inference_model$mcmc$chain_length, # nolint sorry Demeter
    get_razzo_mcmc_chain_length()
  )
})
