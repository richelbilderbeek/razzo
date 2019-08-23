test_that("use", {
  filenames <- save_razzo_paramses(
    create_test_razzo_paramses(
      peregrine::get_pff_tempfile()
    )
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
