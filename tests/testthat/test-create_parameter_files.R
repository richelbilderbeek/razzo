context("test-create_parameters_files")

test_that("use", {

  # Put files in temporary folder
  super_folder_name <- peregrine::get_pff_tempdir()
  project_folder_name <- file.path(super_folder_name, "razzo_project")

  # Do warn if the folder already exists: it should not be
  #dir.create(path = project_folder_name, recursive = TRUE, showWarnings = TRUE)

  filenames <- create_parameters_files(
    project_folder_name = project_folder_name,
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
  super_folder_name <- dirname(peregrine::get_pff_tempfile())
  project_folder_name <- file.path(super_folder_name, "razzo_project")

  # Do warn if the folder already exists: it should not be
  dir.create(path = project_folder_name, recursive = TRUE, showWarnings = TRUE)

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

  # MCMC of full run should be 1111k
  first_filename <- filenames[1]
  first_parameters <- open_parameters_file(first_filename)
  expect_equal(
    first_parameters$pir_params$experiments[[1]]$inference_model$mcmc$chain_length, # nolint yup, Demeter won't be happy about this long line
    1111000
  )
})
test_that("can read", {

  # Put files in temporary folder
  super_folder_name <- peregrine::get_pff_tempdir()
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

