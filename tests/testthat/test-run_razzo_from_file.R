context("test-run_razzo_from_file")

test_that("use", {

  if (!beastier::is_on_travis()) return()

  project_folder_name <- get_razzo_path("razzo_project")
  dir.create(path = project_folder_name, recursive = TRUE, showWarnings = FALSE)
  parameters_filenames <- create_files_razzo_paramses(
    project_folder_name = project_folder_name,
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
    "'parameters_filename' cannot be found"
  )
  expect_error(
    run_razzo_from_file(
      parameters_filename = get_razzo_path("parameters.RDa"),
      add_verbose = "nonsense"
    ),
    "'add_verbose' must be TRUE or FALSE"
  )
})
