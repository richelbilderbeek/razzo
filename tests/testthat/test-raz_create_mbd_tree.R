context("raz_create_mbd_tree")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_tempdir(); # folder_name <- tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Create the parameter files
  razzo:::raz_standard_parameters_interval()
  filenames <- razzo::raz_create_parameters_files(folder_name = folder_name,
                                                  lambda.interval = lambda.interval,
                                                  mu.interval = mu.interval,
                                                  nu.interval = nu.interval,
                                                  q.interval = q.interval,
                                                  seed.interval = seed.interval,
                                                  soc = soc,
                                                  age = age,
                                                  cond = cond,
                                                  sequence_length = sequence_length)

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

test_that("must have as much multiple bursts as predicted by nu", {

  # Work from a folder
  folder_name <- razzo:::raz_tempdir(); # folder_name <- tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Create the parameter files
  filenames <- razzo::raz_create_parameters_files(folder_name = folder_name)
  testthat::expect_true(length(filenames) > 0)
  one_parameter_setting <- dirname(filenames[1])
  testthat::expect_true(file.exists(one_parameter_setting))
  parameters_filename <- file.path(one_parameter_setting, "parameters.csv")
  mbd_tree_filename   <- file.path(one_parameter_setting, "mbd.tree")

  # Get parameters
  parameters <- razzo::raz_open_parameters_file(parameters_filename)

  # Create tree
  razzo::raz_create_mbd_tree(parameters = parameters, folder_name = folder_name)
  testit::assert(file.exists(mbd_tree_filename))

  skip("TODO: Issue #24: create trees with the expected number of events")
  mbd_tree <- ape::read.tree(file = mbd_tree_filename)

  # Calculate the number of expected triggered speciation events
  exp_n_spec_events <- parameters$crown_age / parameters$nu

  # Count the number of actual triggered speciation events
  n_spec_events <- mbd::mbd_count_n_spec_events(mbd_tree)

  expect_equal(exp_n_spec_events, n_spec_events)


})
