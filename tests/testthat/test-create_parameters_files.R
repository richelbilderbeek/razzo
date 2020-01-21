test_that("use", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  filenames <- razzo::create_parameters_files(
    project_folder_name = file.path(
      peregrine::get_pff_tempfile(), "razzo_project"
    ),
    experiment_type = "test" # which is default
  )

  # All filenames must be unique
  expect_equal(length(filenames), length(unique(filenames)))

  # The folder structure created:
  # * razzo_project (the name of the GitHub containing the scripts)
  #   * scripts
  #   * data
  #     * folder named after parameters, e.g. '0.2-0.15-2.5-0.1'
  #       * folder named after seed, e.g. '1'
  #   * figures

  # OK: Parameter filenames end with 'parameters.RDa'
  expect_true(
    length(
      grep(
        pattern = "parameters\\.RDa$", filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )
  # OK: there is a data folder that is a subfolder of razzo_project'
  # Use ..? to indicate one or two back- or normal slashes
  expect_true(
    length(
      grep(
        pattern = "razzo_project..?data",
        filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )

  skip("Expose #270")
  first_razzo_params <- readRDS(filenames[1])
  second_razzo_params <- readRDS(filenames[2])
  expect_true(
    first_razzo_params$pir_params$twinning_params$rng_seed_twin_tree
    != second_razzo_params$pir_params$twinning_params$rng_seed_twin_tree
  )
  expect_true(
    first_razzo_params$pir_params$twinning_params$rng_seed_twin_alignment
    != second_razzo_params$pir_params$twinning_params$rng_seed_twin_alignment
  )
})

test_that("use, full", {

  if (!beastier::is_on_travis()) return()

  filenames <- razzo::create_parameters_files(
    project_folder_name = file.path(
      peregrine::get_pff_tempfile(), "razzo_project"
    ),
    experiment_type = "full"
  )
  # The folder structure created:
  # * razzo_project (the name of the GitHub containing the scripts)
  #   * scripts
  #   * data
  #     * folder named after parameters, e.g. '0.2-0.15-2.5-0.1'
  #       * folder named after seed, e.g. '1'
  #   * figures

  # OK: Parameter filenames end with 'parameters.RDa'
  expect_true(
    length(
      grep(
        pattern = "parameters\\.RDa$", filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )
  # OK: there is a data folder that is a subfolder of razzo_project'
  # Use ..? to indicate one or two back- or normal slashes
  expect_true(
    length(
      grep(
        pattern = "razzo_project..?data",
        filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )

  # Verify #384: MCMC's log are setup correctly
  first_razzo_params <- readRDS(filenames[1])
  for (experiment in first_razzo_params$pir_params$experiments) {
    expect_equal(
      experiment$inference_model$mcmc$tracelog$log_every,
      get_razzo_mcmc_store_every()
    )
    expect_equal(
      experiment$inference_model$mcmc$treelog$log_every,
      get_razzo_mcmc_store_every()
    )
  }

  skip("Expose #270")
  first_razzo_params <- readRDS(filenames[1])
  second_razzo_params <- readRDS(filenames[2])
  expect_true(
    first_razzo_params$pir_params$twinning_params$rng_seed_twin_tree
    != second_razzo_params$pir_params$twinning_params$rng_seed_twin_tree
  )
  expect_true(
    first_razzo_params$pir_params$twinning_params$rng_seed_twin_alignment
    != second_razzo_params$pir_params$twinning_params$rng_seed_twin_alignment
  )
})

test_that("can read", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  # Create parameter files from fresh
  filenames <- razzo::create_parameters_files(
    project_folder_name = file.path(
      peregrine::get_pff_tempfile(), "razzo_project"
    ),
    experiment_type = "test"
  )

  # Load the first one
  expect_silent(
    razzo::open_parameters_file(parameters_filename = filenames[1])
  )

})
