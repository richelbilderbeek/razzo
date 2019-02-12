context("test-run_razzo")

test_that("use", {

  if (!beastier::is_on_travis()) return()

  parameters_filenames <- create_test_parameters_files()
  for (i in seq_along(parameters_filenames)) {
    parameters_filename <- parameters_filenames[i]
    razzo::run_razzo_from_file(parameters_filename)
  }
})
