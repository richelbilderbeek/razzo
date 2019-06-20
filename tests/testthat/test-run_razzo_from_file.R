context("test-run_razzo_from_file")

test_that("use", {

  if (!beastier::is_on_travis()) return()

  # super_folder_name <- get_pff_tempdir()
  # project_folder_name <- file.path(super_folder_name, "razzo_project")
  project_folder_name <- get_razzo_path("razzo_project")
  dir.create(path = project_folder_name, recursive = TRUE, showWarnings = FALSE)
  parameters_filenames <- create_files_razzo_paramses(
    project_folder_name = project_folder_name,
    experiment_type = "test"
  )

  # Only run the first
  for (i in seq_along(parameters_filenames)) {
    expect_silent(run_razzo_from_file(
      parameters_filename = parameters_filenames[i]
    ))
  }
})

test_that("abuse", {

  parameters_filename <- "neverland"
  expect_error(
    run_razzo_from_file(
      parameters_filename = parameters_filename
    )
  )
})
