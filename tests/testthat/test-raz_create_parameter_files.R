context("raz_create_parameters_files")

test_that("use", {
  folder_name <- tempdir()
  filenames <- raz_create_parameters_files(folder_name = folder_name)
  expect_true(length(filenames) >= 1)

  # TODO: Issue #2
  if (1 == 2) {
    expect_true(all(file.exists(filenames)))
  }
})
