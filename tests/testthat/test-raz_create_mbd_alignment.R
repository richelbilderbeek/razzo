context("raz_create_mbd_alignment")

test_that("the file of mbd alignment exists", {

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(dir.exists(folder_name))

  skip("TODO: fix test")
  # # Create the parameter files
  razzo:::raz_save_standard_test_parameters()
  filenames <- razzo:::raz_get_test_filenames()

  testthat::expect_true(length(filenames) > 0)
  one_parameter_setting <- dirname(filenames[1])
  testthat::expect_true(file.exists(one_parameter_setting))

  # Get parameters
  parameters_filename <- file.path(one_parameter_setting, "parameters.csv")
  parameters <- razzo::raz_open_parameters_file(parameters_filename)

  # Get filenames
  mbd_tree_filename   <- razzo::raz_create_filename_mbd_tree(
    parameters = parameters,
    folder_name = folder_name
  )
  mbd_alignment_filename <- razzo::raz_create_filename_mbd_alignment(
    parameters = parameters,
    folder_name = folder_name
  )

  # Create MBD tree
  razzo::raz_create_mbd_tree_file(
    parameters = parameters, folder_name = folder_name)

  testthat::expect_true(
    length(parameters_filename) > 0
  )
  testthat::expect_true(
    length(mbd_alignment_filename) > 0
  )
  razzo::raz_create_mbd_alignment(
    parameters = parameters, folder_name = folder_name)
  testthat::expect_true(file.exists(mbd_alignment_filename))
})
