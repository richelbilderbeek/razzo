context("raz_create_bd_alignment")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_tempdir(); # folder_name <- tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Create the parameter files
  razzo:::raz_load_standard_test_parameters()
  filenames <- razzo::raz_create_parameters_files(folder_name = folder_name,
                                                  lambda.interval = lambda.interval,
                                                  mu.interval = mu.interval,
                                                  nu.interval = nu.interval,
                                                  q.interval = q.interval,
                                                  seed.interval = seed.interval,
                                                  soc = soc,
                                                  age = age,
                                                  cond = cond,
                                                  sequence_length = sequence_length)

  # parameters_filename   <- file.path(folder_name, sub_folder_name, "parameters.csv")
  # bd_alignment_filename <- file.path(folder_name, sub_folder_name, "bd.fasta")

  # TODO: Issue #15: Add 'raz_create_bd_alignment'
  if (1 == 2) {
    raz_create_bd_alignment(parameters_filename)
    expect_true(file.exists(bd_alignment_filename))
  }
})
