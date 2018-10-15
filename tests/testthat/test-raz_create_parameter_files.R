context("raz_create_parameters_files")

test_that("use", {
  filenames <- raz_create_parameters_files()
  expect_true(length(filenames) >= 1)
})
