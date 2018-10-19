context("raz_create_inference_files")

test_that("use", {

  # # Work from a folder
  # folder_name <- tempdir()
  #
  # # Create the parameter files
  # razzo:::raz_load_standard_test_parameters
  # filenames <- raz_create_parameters_files(folder_name = folder_name,
  #                                          lambda.interval = c(0.2, 0.2),
  #                                          mu.interval = c(0.15, 0.15),
  #                                          nu.interval = seq(from = 1, to = 2.5, by = 0.5),
  #                                          q.interval = seq(from = 0.10, to = 0.20, by = 0.05),
  #                                          seed.interval = 1:3,
  #                                          soc = soc,
  #                                          age = age,
  #                                          cond = cond)
  #
  # one_parameter_setting <- paste0(folder_name, "/", dirname(filenames[1]))
  # testthat::expect_true(file.exists(one_parameter_setting))
  # parameters_filename <- file.path(one_parameter_setting, "parameters.csv")
  #
  # # Work on the parameter file and create two FASTA files
  # input_filenames <- raz_create_input_files(parameters_filename)
  # mbd_fasta_filename <- file.path(one_parameter_setting, "mbd.fasta")
  #
  # # Do inference on the first FASTA file
  # inference_filenames <- raz_create_inference_files(
  #   fasta_filename = mbd_fasta_filename
  # )
  #
  # mbd_trees_filename <- file.path(folder_name, sub_folder_name, "mbd.trees")
  # mbd_log_filename <- file.path(folder_name, sub_folder_name, "mbd.log")
  # mbd_mar_lik_filename <- file.path(folder_name, sub_folder_name, "mbd_mar_lik.csv")
  #
  # expect_true(mbd_trees_filename %in% inference_filenames)
  # expect_true(mbd_log_filename %in% inference_filenames)
  # expect_true(mbd_mar_lik_filename %in% inference_filenames)
  #
  # # TODO: Issue #4: 'raz_create_inference_files' must create the inference files
  # if (1 == 2) {
  #   expect_true(file.exists(mbd_trees_filename))
  #   expect_true(file.exists(mbd_log_filename))
  #   expect_true(file.exists(mbd_mar_lik_filename))
  # }
})
