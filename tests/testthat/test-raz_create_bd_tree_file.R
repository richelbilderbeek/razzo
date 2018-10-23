context("raz_create_bd_tree_file")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(dir.exists(folder_name))

  # Create the parameter files
  parameters_filename <- raz_get_path("parameters.csv")
  parameters <- razzo::raz_open_parameters_file(parameters_filename)

  #
  mbd_tree_filename   <- file.path(folder_name, "mbd.tree")


  # Create MBD tree
  skip("TODO: Issue #: interface should work")
  # The interface below should save an MBD tree as 'mbd.tree' in the
  # folder named 'folder_name'.
  #
  # Instead, the function expects subfolders to be present.
  # If the folder structure is important, these folders must be created
  razzo::raz_create_mbd_tree_file(
    parameters = parameters, folder_name = folder_name)

  # Create BD tree
  silent_output <- capture.output(
    razzo::raz_create_bd_tree(
      parameters = parameters, folder_name = folder_name)
  )

  # Actually create a BD tree and save it
  bd_tree_filename <- file.path(one_parameter_setting, "bd.tree")
  testthat::expect_true(file.exists(bd_tree_filename))
})
