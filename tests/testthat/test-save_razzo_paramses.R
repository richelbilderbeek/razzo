test_that("use", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  filenames <- razzo::save_razzo_paramses(
    razzo::create_test_razzo_paramses(
      peregrine::get_pff_tempfile()
    )
  )
  # All files are created
  testthat::expect_true(all(file.exists(filenames)))

  # All filename must end with 'parameters.RDa'
  testthat::expect_equal(
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
      razzo::check_razzo_params(readRDS(filename))
    )
  }
})
