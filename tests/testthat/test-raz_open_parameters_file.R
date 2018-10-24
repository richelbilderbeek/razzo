context("raz_open_parameters_file")

test_that("use", {

  parameters <- razzo::raz_open_parameters_file(raz_get_path("parameters.csv"))
  testthat::expect_true(parameters$lambda > 0.0)
  testthat::expect_true(parameters$mu >= 0.0)
  testthat::expect_true(parameters$nu >= 0.0)
  testthat::expect_true(parameters$q >= 0.0 && parameters$q <= 1.0)
})
