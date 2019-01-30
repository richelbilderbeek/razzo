context("test-create_parameters_files")

test_that("use", {

  # Put files in temporary folder
  super_folder_name <- tempdir()
  project_folder_name <- file.path(super_folder_name, "razzo_project")

  # Do not warn if the folder already exists
  dir.create(path = project_folder_name, showWarnings = FALSE)

  filenames <- create_parameters_files(
    project_folder_name = project_folder_name
  )

  # The folder structure created:
  # * razzo_project (the name of the GitHub containing the scripts)
  #   * scripts
  #   * data
  #     * folder named after parameters, e.g. '0.2-0.15-2.5-0.1'
  #       * folder named after seed, e.g. '1'
  #   * figures

  # OK: Parameter filenames end with 'parameters.csv'
  testthat::expect_true(
    length(
      grep(
        pattern = "parameters\\.RDa$", filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )
  # OK: there is a data folder that is a subfolder of razzo_project'
  # Use ..? to indicate one or two back- or normal slashes
  testthat::expect_true(
    length(
      grep(
        pattern = "razzo_project..?data",
        filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )

})

test_that("use, full", {

  # Put files in temporary folder
  super_folder_name <- tempdir()
  project_folder_name <- file.path(super_folder_name, "razzo_project")

  # Do not warn if the folder already exists
  dir.create(path = project_folder_name, showWarnings = FALSE)

  filenames <- create_parameters_files(
    project_folder_name = project_folder_name,
    experiment_type = "full"
  )

  # The folder structure created:
  # * razzo_project (the name of the GitHub containing the scripts)
  #   * scripts
  #   * data
  #     * folder named after parameters, e.g. '0.2-0.15-2.5-0.1'
  #       * folder named after seed, e.g. '1'
  #   * figures

  # OK: Parameter filenames end with 'parameters.csv'
  testthat::expect_true(
    length(
      grep(
        pattern = "parameters\\.RDa$", filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )
  # OK: there is a data folder that is a subfolder of razzo_project'
  # Use ..? to indicate one or two back- or normal slashes
  testthat::expect_true(
    length(
      grep(
        pattern = "razzo_project..?data",
        filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )

})
test_that("can read", {

  # Create parameter files from fresh
  filenames <- create_parameters_files(
    project_folder_name = tempdir()
  )

  # Load the first one
  expect_silent(
    open_parameters_file(parameters_filename = filenames[1])
  )
})
