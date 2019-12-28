test_that("use", {

  skip("Fix https://github.com/richelbilderbeek/razzo/issues/375")

  # If ''filename' must be the name of a file in 'inst/extdata'', do
  x <- razzo::get_data_paths(
    project_folder_name = razzo::get_razzo_path("razzo_project/")
  )
  parameters <- razzo::open_parameters_file(
    parameters_filename = mbd::file_path(x[[1]], "parameters.RDa")
  )
  expect_true(
    all(c("mbd_params", "pir_params", "misc_params") %in% names(parameters))
  )
  expect_silent(razzo::check_mbd_params(parameters$mbd_params))
  expect_silent(pirouette::check_pir_params(parameters$pir_params))
  expect_silent(razzo::check_misc_params(parameters$misc_params))
})
