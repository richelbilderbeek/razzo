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
