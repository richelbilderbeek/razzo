test_that("use", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  filenames <- create_parameters_files(
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
  if (!beastier::is_on_travis()) return()

  filenames <- create_parameters_files(
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

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  # Create parameter files from fresh
  filenames <- create_parameters_files(
    project_folder_name = file.path(
      peregrine::get_pff_tempfile(), "razzo_project"
    ),
    experiment_type = "test"
  )

  # Load the first one
  expect_silent(
    open_parameters_file(parameters_filename = filenames[1])
  )
})
