test_that("use", {
  project_folder_name <- peregrine::get_pff_tempdir()
  razzo_paramses <- create_razzo_paramses(
    project_folder_name = project_folder_name
  )
  for (razzo_params in razzo_paramses) {
    check_razzo_params(razzo_params)
  }
})
