test_that("use", {

  # If ''filename' must be the name of a file in 'inst/extdata'', do
  if (1 == 2) {
    # this functon does not exists
    create_files_razzo_params(
      project_folder_name = system.file(
        "extdata",
        "razzo_project",
        package = "razzo"
      ),
      experiment_type = "test"
    )
  }
  x <- razzo::get_data_paths(
    project_folder_name = razzo::get_razzo_path("razzo_project/")
  )
  parameters <- razzo::open_parameters_file(
    parameters_filename = mbd::file_path(x[[1]], "parameters.RDa")
  )
  testthat::expect_true(
    all(c("mbd_params", "pir_params", "misc_params") %in% names(parameters))
  )
  testthat::expect_silent(razzo::check_mbd_params(parameters$mbd_params))
  testthat::expect_silent(pirouette::check_pir_params(parameters$pir_params))
  testthat::expect_silent(razzo::check_misc_params(parameters$misc_params))
})
