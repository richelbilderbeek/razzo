context("test-run_razzo_from_file")

test_that("use", {

  if (!beastier::is_on_travis()) return()

  super_folder_name <- tempdir()
  project_folder_name <- file.path(super_folder_name, "razzo_project")
  dir.create(path = project_folder_name, recursive = TRUE, showWarnings = FALSE)
  parameters_filenames <- create_parameters_files(
    project_folder_name = project_folder_name
  )
  parameters_filename <- parameters_filenames[1]
  razzo::run_razzo_from_file(razzo_input_filename = parameters_filename)
})