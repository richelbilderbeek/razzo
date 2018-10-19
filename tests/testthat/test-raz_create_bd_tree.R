context("raz_create_bd_tree")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_tempdir(); # folder_name <- tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Create the parameter files
  razzo:::raz_load_standard_test_parameters()
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

  # Create MBD tree
  razzo::raz_create_mbd_tree(parameters = parameters, folder_name = folder_name)

  # Create BD tree
  silent_output <- capture.output(
    razzo::raz_create_bd_tree(parameters = parameters, folder_name = folder_name)
  )

  # Actually create a BD tree and save it
  bd_tree_filename <- file.path(one_parameter_setting, "bd.tree")
  testthat::expect_true(file.exists(bd_tree_filename))
})
