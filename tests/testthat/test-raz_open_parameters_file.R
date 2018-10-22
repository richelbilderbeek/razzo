context("raz_open_parameters_file")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  skip("TODO: fix test")

  # Create the parameter files
  razzo:::raz_save_standard_test_parameters()
  filenames <- razzo:::raz_get_test_filenames()

  filename <- filenames[1]
  parameters <- razzo::raz_open_parameters_file(filename)
  testthat::expect_true(parameters$lambda > 0.0)
  testthat::expect_true(parameters$mu >= 0.0)
  testthat::expect_true(parameters$nu >= 0.0)
  testthat::expect_true(parameters$q >= 0.0 && parameters$q <= 1.0)
})
