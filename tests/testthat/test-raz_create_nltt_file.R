context("raz_create_nltt_file")

test_that("use", {

  skip("TODO")

  # Work from a folder
  folder_name <- tempdir()

  # Create the parameter files
  raz_create_parameters_files(folder_name)
  sub_folder_name <- "1"
  parameters_filename <- file.path(
    folder_name, sub_folder_name, "parameters.csv")
  # Work on the parameter file and create two FASTA files
  input_filenames <- raz_create_input_files(parameters_filename)
  mbd_fasta_filename <- file.path(folder_name, sub_folder_name, "mbd.fasta")
  mbd_tree_filename <- file.path(folder_name, sub_folder_name, "mbd.tree")
  # Do inference on the first MBD trees
  posterior_filenames <- raz_create_posterior_files(
    fasta_filename = mbd_fasta_filename
  )
  mbd_trees_filename <- file.path(folder_name, sub_folder_name, "mbd.trees")

  # Start real work
  nltt_filename <- raz_create_nltt_file(
    trees_filename = mbd_trees_filename
  )
  expect_equal(file.path(folder_name, sub_folder_name, "mbd_nltts.csv"),
    nltt_filename)

  # TODO: Issue #5: 'raz_create_nltt_file' must create an nLTT file
  if (1 == 2) {
    expect_true(file.exists(nltt_filename))
  }
})
