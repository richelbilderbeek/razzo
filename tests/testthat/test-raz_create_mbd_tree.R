context("raz_create_mbd_tree")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Get parameters
  parameters_filename <- raz_get_path("parameters.csv")
  testit::assert(file.exists(parameters_filename))
  parameters <- razzo::raz_open_parameters_file(parameters_filename)

  # Create tree
  skip("TODO: Issue #: this interface should work")
  # The interface below should save an MBD tree as 'mbd.tree' in the
  # folder named 'folder_name'.
  #
  # Instead, the function expects subfolders to be present.
  # If the folder structure is important, these folders must be created
  razzo::raz_create_mbd_tree(parameters = parameters, folder_name = folder_name)
  # This line below may not be true if the subfolder structure is important
  mbd_tree_filename <- file.path(folder_name, "mbd.tree")

  # Actually create an MBD tree and save it
  testthat::expect_true(file.exists(mbd_tree_filename))

})

test_that("must have as much multiple bursts as predicted by nu", {

  # Work from a folder
  folder_name <- razzo:::raz_tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Get parameters
  parameters_filename <- raz_get_path("parameters.csv")
  testit::assert(file.exists(parameters_filename))
  parameters <- razzo::raz_open_parameters_file(parameters_filename)

  # Create tree
  skip("TODO: Issue #: this interface should work")
  # The interface below should save an MBD tree as 'mbd.tree' in the
  # folder named 'folder_name'.
  #
  # Instead, the function expects subfolders to be present.
  # If the folder structure is important, these folders must be created
  razzo::raz_create_mbd_tree(parameters = parameters, folder_name = folder_name)
  # This line below may not be true if the subfolder structure is important
  mbd_tree_filename <- file.path(folder_name, "mbd.tree")
  testit::assert(file.exists(mbd_tree_filename))

  skip("TODO: Issue #24: create trees with the expected number of events")
  mbd_tree <- ape::read.tree(file = mbd_tree_filename)

  # Calculate the number of expected triggered speciation events
  exp_n_spec_events <- parameters$crown_age / parameters$nu

  # Count the number of actual triggered speciation events
  n_spec_events <- mbd::mbd_count_n_spec_events(mbd_tree)

  expect_equal(exp_n_spec_events, n_spec_events)


})
