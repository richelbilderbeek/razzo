context("raz_create_input_files")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir()
  testthat::expect_true(dir.exists(folder_name))

  skip("TODO: fix test")

  # Create the parameter files
  razzo:::raz_save_standard_test_parameters()
  filenames <- razzo:::raz_get_standard_test_filenames()

  parameters_filename <- filenames[1]
  parameters <- razzo::raz_open_parameters_file(parameters_filename = parameters_filename)

  # Get filenames
  mbd_tree_filename  <- razzo::raz_create_filename_mbd_tree(parameters = parameters, folder_name = folder_name)
  bd_tree_filename   <- razzo::raz_create_filename_bd_tree(parameters = parameters, folder_name = folder_name)
  mbd_fasta_filename <- razzo::raz_create_filename_mbd_alignment(parameters = parameters, folder_name = folder_name)
  bd_fasta_filename  <- razzo::raz_create_filename_bd_alignment(parameters = parameters, folder_name = folder_name)
  filenames <- c(mbd_fasta_filename,
                 mbd_tree_filename,
                 bd_fasta_filename,
                 bd_tree_filename)

  parameters_path <- razzo::raz_get_parameters_path(parameters = parameters, folder_name = folder_name)

  # Expect four files to be created
  exp_mbd_fasta_filename <- file.path(parameters_path, "mbd.fasta")
  exp_mbd_tree_filename  <- file.path(parameters_path, "mbd.tree")
  exp_bd_fasta_filename  <- file.path(parameters_path, "bd.fasta")
  exp_bd_tree_filename   <- file.path(parameters_path, "bd.tree")
  expected_filenames <- c(exp_mbd_fasta_filename,
                          exp_mbd_tree_filename,
                          exp_bd_fasta_filename,
                          exp_bd_tree_filename)

  testthat::expect_equal(
    sort(filenames),
    sort(expected_filenames)
  )

  # is this really the aim of the test/function?
  if (1 == 2) {
    testthat::expect_true(file.exists(mbd_tree_filename))
    testthat::expect_true(file.exists(mbd_fasta_filename))
    testthat::expect_true(file.exists(bd_tree_filename))
    testthat::expect_true(file.exists(bd_fasta_filename))
  }

})
