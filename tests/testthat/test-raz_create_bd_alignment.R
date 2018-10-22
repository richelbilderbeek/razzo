context("raz_create_bd_alignment")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir(); # folder_name <- tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Create the parameter files
  razzo:::raz_save_standard_test_parameters()
  filenames <- razzo:::raz_get_standard_test_filenames()

  # parameters_filename   <- file.path(folder_name, sub_folder_name, "parameters.csv")
  # bd_alignment_filename <- file.path(folder_name, sub_folder_name, "bd.fasta")

  # TODO: Issue #15: Add 'raz_create_bd_alignment'
  if (1 == 2) {
    raz_create_bd_alignment(parameters_filename)
    expect_true(file.exists(bd_alignment_filename))
  }
})
