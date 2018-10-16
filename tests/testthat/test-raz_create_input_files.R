context("raz_create_input_files")

test_that("use", {
  # Work from a folder
  folder_name <- tempdir()

  # Create the parameter files
  raz_create_parameters_files(folder_name)
  sub_folder_name <- "1"
  parameters_filename <- file.path(folder_name, sub_folder_name, "parameters.csv")

  # Work on the parameter file
  input_filenames <- raz_create_input_files(parameters_filename)

  # Expect four files to be created
  mbd_fasta_filename <- file.path(folder_name, sub_folder_name, "mbd.fasta")
  mbd_tree_filename <- file.path(folder_name, sub_folder_name, "mbd.tree")
  bd_fasta_filename <- file.path(folder_name, sub_folder_name, "bd.fasta")
  bd_tree_filename <- file.path(folder_name, sub_folder_name, "bd.tree")

  # TODO: Issue #3
  if (1 == 2) {
    expect_equal(file.exists(mbd_fasta_filename))
    expect_equal(file.exists(mbd_tree_filename))
    expect_equal(file.exists(bd_fasta_filename))
    expect_equal(file.exists(bd_tree_filename))
  }
})
