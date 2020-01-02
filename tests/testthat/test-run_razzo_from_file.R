context("test-run_razzo_from_file")

test_that("use", {

  if (!beastier::is_on_travis()) return()

  parameters_filenames <- create_parameters_files(
    project_folder_name = file.path(
      peregrine::get_pff_tempdir(),
      "razzo_project"
    ),
    experiment_type = "test"
  )

  parameters <- open_parameters_file(parameters_filenames[1])
  expect_false(
    parameters$pir_params$experiments[[1]]$est_evidence_mcmc$treelog$filename ==
    parameters$pir_params$experiments[[1]]$inference_model$mcmc$treelog$filename
  )
  skip("#376")
  expect_false(
    parameters$pir_params$experiments[[1]]$est_evidence_mcmc$treelog$filename ==
    "$(tree).trees"
  )
  unlist(parameters)[
    !is.na(
      stringr::str_match(string = names(unlist(parameters)), pattern = "filename")
    )
  ]

  # Run the first without verbose
  expect_silent(
    run_razzo_from_file(
      parameters_filename = parameters_filenames[1]
    )
  )

  # Run the second verbosely
  expect_output(
    run_razzo_from_file(
      parameters_filename = parameters_filenames[2],
      add_verbose = TRUE
    )
  )


})

test_that("abuse", {

  parameters_filename <- "neverland"
  expect_error(
    run_razzo_from_file(
      parameters_filename = parameters_filename
    ),
    "File 'parameters_filename' not found"
  )
  expect_error(
    run_razzo_from_file(
      parameters_filename = get_razzo_path("parameters.RDa"),
      add_verbose = "nonsense"
    ),
    "'add_verbose' must be one boolean"
  )
})




test_that("use", {

  if (!beastier::is_on_travis()) return()

  skip("Expose #350")

  parameters_filenames <- create_parameters_files(
    project_folder_name = file.path(
      peregrine::get_pff_tempdir(),
      "razzo_project"
    ),
    experiment_type = "test"
  )
  parameters_filename <- parameters_filenames[1]

  # Run the first without verbose
  expect_silent(
    run_razzo_from_file(
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
  expect_equal(
    expected_mcmc_chain_length,
    actual_mcmc_chain_length
  )
})
