test_that("use", {
  project_folder_name <- peregrine::get_pff_tempdir()
  filenames <- save_razzo_paramses(
    project_folder_name = project_folder_name
  )
  # All files are created
  expect_true(all(file.exists(filenames)))

  # All files are valid
  for (filename in filenames) {
    testthat::expect_silent(
      check_razzo_params(readRDS(filename))
    )
  }
})
