context("raz_create_mbd_tree")

test_that("use", {

  # Work from a folder
  folder_name <- tempdir()
  # Create the parameter files
  raz_create_parameters_files(folder_name)
  sub_folder_name <- "1"
  parameters_filename <- file.path(folder_name, sub_folder_name, "parameters.csv")
  mbd_tree_filename <- file.path(folder_name, sub_folder_name, "mbd.tree")

  parameters <- raz_open_parameters_file(parameters_filename)

  # No tree present yet
  testit::assert(!file.exists(mbd_tree_filename))

  # Create tree
  raz_create_mbd_tree(parameters, mbd_tree_filename)

  # TODO: Issue #8: actually create an MBD tree and save it
  if (1 == 2) {
    expect_true(file.exists(mbd_tree_filename))
  }
})
