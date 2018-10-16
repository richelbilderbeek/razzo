context("raz_create_bd_tree")

test_that("use", {

  # Work from a folder
  folder_name <- tempdir()

  # Create the parameter files
  soc <- 2
  age <- 10
  cond <- 1
  filenames <- raz_create_parameters_files(folder_name = folder_name,
                                           lambda.interval = c(0.2, 0.2),
                                           mu.interval = c(0.15, 0.15),
                                           nu.interval = seq(from = 1, to = 2.5, by = 0.5),
                                           q.interval = seq(from = 0.10, to = 0.20, by = 0.05),
                                           seed.interval = 1:3,
                                           soc = soc,
                                           age = age,
                                           cond = cond)

  one_parameter_setting <- paste0(folder_name, "/", dirname(filenames[1]))
  testthat::expect_true(file.exists(one_parameter_setting))

  parameters_filename <- file.path(one_parameter_setting, "parameters.csv")
  mbd_tree_filename <- file.path(one_parameter_setting, "mbd.tree")

  # Get parameters
  parameters <- raz_open_parameters_file(parameters_filename)

  # Create MBD tree
  raz_create_mbd_tree(parameters = parameters, mbd_tree_filename = mbd_tree_filename)

  # Create BD tree
  raz_create_bd_tree(mbd_tree_filename = mbd_tree_filename)

  # Actually create a BD tree and save it
  bd_tree_filename <- file.path(one_parameter_setting, "bd.tree")
  testthat::expect_true(file.exists(bd_tree_filename))
})
