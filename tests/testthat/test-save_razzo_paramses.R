test_that("use", {
  filenames <- save_razzo_paramses(
    create_test_razzo_paramses(
      peregrine::get_pff_tempfile()
    )
  )
  # All files are created
  expect_true(all(file.exists(filenames)))

  # All filename must end with 'parameters.RDa'
  expect_equal(
    length(filenames),
    length(
      grep(
        pattern = "parameters\\.RDa$", filenames, perl = TRUE, value = TRUE
      )
    )
  )

  # All files are valid
  for (filename in filenames) {
    testthat::expect_silent(
      check_razzo_params(readRDS(filename))
    )
  }
})
