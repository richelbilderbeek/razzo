context("raz_create_mbd_tree")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir(); # folder_name <- tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Create the parameter files
  razzo:::raz_save_standard_test_parameters()
  filenames <- razzo:::raz_get_standard_test_filenames()

  testthat::expect_true(length(filenames) > 0)
  one_parameter_setting <- dirname(filenames[1])
  testthat::expect_true(file.exists(one_parameter_setting))
  parameters_filename <- file.path(one_parameter_setting, "parameters.csv")
  mbd_tree_filename   <- file.path(one_parameter_setting, "mbd.tree")

  # Get parameters
  parameters <- razzo::raz_open_parameters_file(parameters_filename)

  # Create tree
  razzo::raz_create_mbd_tree(parameters = parameters, folder_name = folder_name)

  # Actually create an MBD tree and save it
  testthat::expect_true(file.exists(mbd_tree_filename))

})
