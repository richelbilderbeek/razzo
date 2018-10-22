context("raz_create_bd_tree")

test_that("use", {

  # Work from a temporary folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  mbd_tree_filename   <- file.path(folder_name, "mbd.tree")

  # Get parameters
  parameters_filename <- raz_get_path("parameters.csv")
  testit::assert(file.exists(parameters_filename))
  parameters <- razzo::raz_open_parameters_file(parameters_filename)

  # Create MBD tree
  skip("TODO: Issue #: this interface should work")
  # This interface should work: with parameters and a target folder,
  # an MBD tree can be created and saved as 'mbd.tree' in that folder.
  #
  # In this case, I see the target folder is not just 'folder_name',
  # but 'folder_name/0.2-0.15-1-0.1/1'. If that folder structure is
  # important, apparently 'raz_create_mbd_tree' needs to create those subfolders
  razzo::raz_create_mbd_tree(parameters = parameters, folder_name = folder_name)

  # Create BD tree
  silent_output <- capture.output(
    razzo::raz_create_bd_tree(parameters = parameters, folder_name = folder_name)
  )

  # Actually create a BD tree and save it
  bd_tree_filename <- file.path(one_parameter_setting, "bd.tree")
  testthat::expect_true(file.exists(bd_tree_filename))
})
