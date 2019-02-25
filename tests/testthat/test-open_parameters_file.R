context("test-open_parameters_file")

test_that("use", {

  parameters <- open_parameters_file(
    parameters_filename = get_path("parameters.RDa")
  )
  expect_true(parameters$lambda > 0.0)
  expect_true(parameters$mu >= 0.0)
  expect_true(parameters$nu >= 0.0)
  expect_true(parameters$q >= 0.0 && parameters$q <= 1.0)
  expect_true(!is.null(parameters$clock_model))
  expect_true(parameters$clock_model == "strict" ||
      parameters$clock_model == "rln")
  expect_true(!is.null(parameters$site_model))
  expect_true(parameters$site_model == "jc69" ||
      parameters$site_model == "gtr")
})
