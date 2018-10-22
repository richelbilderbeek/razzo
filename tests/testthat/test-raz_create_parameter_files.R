context("raz_create_parameters_files")

test_that("use", {

  # Put files in temporary folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  skip("TODO: fix test")

  # Create the parameter files
  razzo:::raz_save_standard_test_parameters()
  # filenames <- razzo:::raz_get_standard_test_filenames()

  testthat::expect_true(length(filenames) >= 1)
  testthat::expect_true(all(file.exists(filenames)))

  skip("TODO: Issue #: use the folder structure agreed up")
  # The folder structure we agreed upon:
  # * razzo_project
  #   * scripts
  #   * data
  #     * folder named after parameters, e.g. '0.2-0.15-2.5-0.1'
  #       * folder named after seed, e.g. '1'
  #   * figures

  # OK: Parameter filenames end with 'parameters.csv'
  testthat::expect_true(
    length(
      grep(
        pattern = "parameters\\.csv$", filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )
  # Not OK: there should be a data folder
  # (note: simply replace 'razzo_project' by 'data' :-) )
  testthat::expect_true(
    length(
      grep(
        pattern = "/data/", filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )

})
