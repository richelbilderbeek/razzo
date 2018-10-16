context("raz_create_inference_files")

test_that("use", {

  # Work from a folder
  folder_name <- tempdir()

  # Create the parameter files
  raz_create_parameters_files(folder_name)
  sub_folder_name <- "1"
  parameters_filename <- file.path(folder_name, sub_folder_name, "parameters.csv")
  # Work on the parameter file and create two FASTA files
  input_filenames <- raz_create_input_files(parameters_filename)
  mbd_fasta_filename <- file.path(folder_name, sub_folder_name, "mbd.fasta")

  # Do inference on the first FASTA file
  inference_filenames <- raz_create_inference_files(
    fasta_filename = mbd_fasta_filename
  )

  mbd_trees_filename <- file.path(folder_name, sub_folder_name, "mbd.trees")
  mbd_log_filename <- file.path(folder_name, sub_folder_name, "mbd.log")
  mbd_mar_lik_filename <- file.path(folder_name, sub_folder_name, "mbd_mar_lik.csv")

  expect_true(mbd_trees_filename %in% inference_filenames)
  expect_true(mbd_log_filename %in% inference_filenames)
  expect_true(mbd_mar_lik_filename %in% inference_filenames)

  # TODO: Issue #4: 'raz_create_inference_files' must create the inference files
  if (1 == 2) {
    expect_true(file.exists(mbd_trees_filename))
    expect_true(file.exists(mbd_log_filename))
    expect_true(file.exists(mbd_mar_lik_filename))
  }
})
