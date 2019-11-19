test_that("use", {

  # If ''filename' must be the name of a file in 'inst/extdata'', do
  if (1 == 2) {
    create_files_razzo_params(
      project_folder_name = system.file(
        "extdata",
        "razzo_project",
        package = "razzo"
      ),
      experiment_type = "test"
    )
  }
  x <- get_data_paths(project_folder_name = get_razzo_path("razzo_project/"))
  parameters <- open_parameters_file(
    parameters_filename = file_path(x[[1]], "parameters.RDa")
  )
  expect_true(
    all(c("mbd_params", "pir_params", "misc_params") %in% names(parameters))
  )
  expect_silent(check_mbd_params(parameters$mbd_params))
  expect_silent(pirouette::check_pir_params(parameters$pir_params))
  expect_silent(check_misc_params(parameters$misc_params))
})
