context("raz_create_parameters_files")

test_that("use", {

  # Put files in temporary folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(dir.exists(folder_name))

  filenames <- raz_create_parameters_files(folder_name = folder_name)

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
        pattern = "parameters\\.csv$", filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )
  # OK: there is a data folder
  testthat::expect_true(
    length(
      grep(
        pattern = "/data/", filenames[1], perl = TRUE, value = TRUE
      )
    ) > 0
  )

})
