context("raz_create_bd_alignment")

test_that("use", {

  # Work from a folder
  folder_name <- tempdir()

  # Create the parameter files
  raz_create_parameters_files(folder_name)
  sub_folder_name <- "1"
  parameters_filename <- file.path(folder_name, sub_folder_name, "parameters.csv")
  bd_alignment_filename <- file.path(folder_name, sub_folder_name, "bd.fasta")

  # TODO: Issue #15: Add 'raz_create_bd_alignment'
  if (1 == 2) {
    raz_create_bd_alignment(parameters_filename)
    expect_true(file.exists(bd_alignment_filename))
  }
})
