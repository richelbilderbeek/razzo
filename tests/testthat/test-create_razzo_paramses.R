test_that("use", {
  project_folder_name <- peregrine::get_pff_tempdir()
  razzo_paramses <- create_razzo_paramses(
    project_folder_name = project_folder_name
  )
  for (razzo_params in razzo_paramses) {
    check_razzo_params(razzo_params)
  }
})

test_that("matches article", {
  # Issue 242, Isssue #242
  #
  # The mbd_params are checked elsewhere
  # The pir_params are checked elsewhere
  # The misc_params are irrelevant to be checked :-)
  #
  # Search for 'matches article' to find these
  #
  project_folder_name <- peregrine::get_pff_tempdir()

  n_replicates <- 1
  mbd_params_table <- create_mbd_params_table(
    n_replicates = n_replicates
  )
  n_mbd_params <- nrow(mbd_params_table)

  razzo_paramses <- create_razzo_paramses(
    project_folder_name = project_folder_name,
    mbd_paramses = mbd_params_table
  )
  n_razzo_paramses <- length(razzo_paramses)
  expect_equal(n_mbd_params, n_razzo_paramses)
})
