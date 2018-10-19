context("raz_open_parameters_file")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir(); # folder_name <- tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Create the parameter files
  razzo:::raz_load_standard_test_parameters()
  filenames <- razzo::raz_create_parameters_files(folder_name = folder_name,
                                                  lambda.interval = lambda.interval,
                                                  mu.interval = mu.interval,
                                                  nu.interval = nu.interval,
                                                  q.interval = q.interval,
                                                  seed.interval = seed.interval,
                                                  soc = soc,
                                                  age = age,
                                                  cond = cond,
                                                  sequence_length = sequence_length)

  filename <- filenames[1]
  parameters <- razzo::raz_open_parameters_file(filename)
  testthat::expect_true(parameters$lambda > 0.0)
  testthat::expect_true(parameters$mu >= 0.0)
  testthat::expect_true(parameters$nu >= 0.0)
  testthat::expect_true(parameters$q >= 0.0 && parameters$q <= 1.0)
})
