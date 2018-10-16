context("raz_open_parameters_file")

test_that("use", {

  folder_name <- tempdir()
  filenames <- raz_create_parameters_files(folder_name = folder_name)
  filename <- filenames[1]
  parameters <- raz_open_parameters_file(filename)
  expect_true(parameters$lambda > 0.0)
})
