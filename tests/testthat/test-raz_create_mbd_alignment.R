context("raz_create_mbd_alignment")

test_that("use", {

  # Work from a folder
  folder_name <- tempdir()

  # Create the parameter files
  raz_create_parameters_files(folder_name)
  sub_folder_name <- "1"
  parameters_filename <- file.path(folder_name, sub_folder_name, "parameters.csv")
  mbd_alignment_filename <- file.path(folder_name, sub_folder_name, "mbd.fasta")

  # TODO: Issue #14: Add 'raz_create_mbd_alignment'
  if (1 == 2) {
    raz_create_mbd_alignment(parameters_filename)
    expect_true(file.exists(mbd_alignment_filename))
  }
})
