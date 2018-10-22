context("raz_create_parameters_files")

test_that("use", {

  # Put files in temporary folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Create the parameter files
  razzo:::raz_save_standard_test_parameters()
  filenames <- razzo:::raz_get_standard_test_filenames()

  testthat::expect_true(length(filenames) >= 1)
  testthat::expect_true(all(file.exists(filenames)))
})
