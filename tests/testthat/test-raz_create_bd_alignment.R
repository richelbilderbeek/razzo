context("raz_create_bd_alignment")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(dir.exists(folder_name))

  parameters <- razzo::raz_open_parameters_file(raz_get_path("parameters.csv"))

  skip("TODO: Issue #15: Add 'raz_create_bd_alignment'")
  # bd_alignment_filename <- file.path(folder_name, sub_folder_name, "bd.fasta")

  if (1 == 2) {
    raz_create_bd_alignment(parameters_filename)
    expect_true(file.exists(bd_alignment_filename))
  }
})
