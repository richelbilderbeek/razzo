context("test-run_razzo_from_file")

test_that("use", {

  if (!beastier::is_on_travis()) return()
  skip("Too long")

  parameters_filenames <- razzo::create_parameters_files(
    project_folder_name = file.path(
      peregrine::get_pff_tempdir(),
      "razzo_project"
    ),
    experiment_type = "test"
  )

  testthat::expect_true(
    razzo::open_parameters_file(
      parameters_filenames[1]
    )$pir_params$experiments[[1]]$est_evidence_mcmc$treelog$filename !=
    "$(tree).trees"
  )

  # Run the first verbosely
  testthat::expect_output(
    razzo::run_razzo_from_file(
      parameters_filename = parameters_filenames[2],
      add_verbose = TRUE
    )
  )

  # Run the second without verbose
  testthat::expect_silent(
    razzo::run_razzo_from_file(
      parameters_filename = parameters_filenames[1]
    )
  )

})

test_that("abuse", {

  parameters_filename <- "neverland"
  testthat::expect_error(
    razzo::run_razzo_from_file(
      parameters_filename = parameters_filename
    ),
    "File 'parameters_filename' not found"
  )
  testthat::expect_error(
    razzo::run_razzo_from_file(
      parameters_filename = raztr::get_raztr_path("parameters.RDa"),
      add_verbose = "nonsense"
    ),
    "'add_verbose' must be one boolean"
  )
})

test_that("use", {

  if (!beastier::is_on_travis()) return()

  skip("Expose #350")

  parameters_filenames <- razzo::create_parameters_files(
    project_folder_name = file.path(
      peregrine::get_pff_tempdir(),
      "razzo_project"
    ),
    experiment_type = "test"
  )
  parameters_filename <- parameters_filenames[1]

  # Run the first without verbose
  testthat::expect_silent(
    razzo::run_razzo_from_file(
      parameters_filename = parameters_filename
    )
  )

  razzo_params <- readRDS(parameters_filename)

  # Check if MCMC chain length in parameters matches the final state
  # in the BEAST2 output files
  expected_mcmc_chain_length <-
    razzo_params$pir_params$experiments[[1]]$inference_model$mcmc$chain_length

  final_mcmc_state_line <- tail(
    readLines(
      razzo_params$pir_params$experiments[[1]]$inference_model$mcmc$treelog$filename, # nolint sorry Demeter
      warn = FALSE
    ),
    n = 2
  )[1]
  final_mcmc_state_line
  actual_mcmc_chain_length <- as.numeric(
    stringr::str_match(
      string = final_mcmc_state_line,
      pattern = "tree STATE_(.*) = \\("
    )[1, 2]
  )
  testthat::expect_equal(
    expected_mcmc_chain_length,
    actual_mcmc_chain_length
  )
})
