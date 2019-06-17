context("test-create_parameters_files")

test_that("must be in working directory by default", {
  # Reasons are among others
  # * The scripts in razzo_project expects this
  # * Writing to inst/extdata is a bad idea
  #
  filenames <- create_parameters_files()
  expected_pattern <- paste0(getwd(), "/data/.*parameters\\.RDa")
  expect_true(
    all(length(grep(x = filenames, pattern = expected_pattern)) >= 1)
  )
})

test_that("use", {

  # Put files in temporary folder
  super_folder_name <- get_pff_tempdir()
  project_folder_name <- file.path(super_folder_name, "razzo_project")

  # Do not warn if the folder already exists
  dir.create(path = project_folder_name, recursive = TRUE, showWarnings = TRUE)

  filenames <- create_parameters_files(
    project_folder_name = project_folder_name
  )

  # All filenames must be unique, Issue 170, #170
  expect_equal(length(filenames), length(unique(filenames)))

  # The folder structure created:
  # * razzo_project (the name of the GitHub containing the scripts)
  #   * scripts
  #   * data
  #     * folder named after parameters, e.g. '0.2-0.15-2.5-0.1'
  #       * folder named after seed, e.g. '1'
  #   * figures

  # OK: Parameter filenames end with 'parameters.RDa'
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

  skip("Issue 127, #127")
  # Put files in temporary folder
  super_folder_name <- dirname(get_pff_tempfile())
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

  # OK: Parameter filenames end with 'parameters.RDa'
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

  # Put files in temporary folder
  super_folder_name <- get_pff_tempdir()
  project_folder_name <- file.path(super_folder_name, "razzo_project")

  # Do not warn if the folder already exists
  dir.create(path = project_folder_name, recursive = TRUE, showWarnings = TRUE)

  # Create parameter files from fresh
  filenames <- create_parameters_files(
    project_folder_name = project_folder_name
  )

  # Load the first one
  expect_silent(
    open_parameters_file(parameters_filename = filenames[1])
  )
})

