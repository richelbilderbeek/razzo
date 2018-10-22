context("raz_create_inference_files")

test_that("use", {

  if (rappdirs::app_dir()$os == "win") {
    skip("Cannot do inference on Windows computers")
    # See https://github.com/richelbilderbeek/beastier/blob/master/doc/faq.md ,
    # section 'Why doesn't beastier support calling the Windows BEAST2.exe
    # file?'. Spoiler: because BEAST2.exe does not allow scripted use
  }

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  skip("TODO: fix test")
  # # Create the parameter files
  razzo:::raz_save_standard_test_parameters()
  filenames <- razzo:::raz_get_standard_test_filenames()

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
  raz_create_mbd_alignment(parameters = parameters, folder_name = folder_name)
  testthat::expect_true(file.exists(mbd_alignment_filename))



  # Work on the parameter file and create two FASTA files
  input_filenames <- raz_create_input_files(parameters_filename)
  mbd_fasta_filename <- file.path(one_parameter_setting, "mbd.fasta")

  # Do inference on the first FASTA file
  inference_filenames <- raz_create_inference_files(
    fasta_filename = mbd_fasta_filename
  )

  mbd_trees_filename <- file.path(folder_name, sub_folder_name, "mbd.trees")
  mbd_log_filename <- file.path(folder_name, sub_folder_name, "mbd.log")
  mbd_mar_lik_filename <- file.path(
    folder_name, sub_folder_name, "mbd_mar_lik.csv")

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
