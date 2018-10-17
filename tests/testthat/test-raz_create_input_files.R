context("raz_create_input_files")

test_that("use", {

  # Work from a folder
  folder_name <- razzo:::raz_tempdir(); # folder_name <- tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Create the parameter files
  lambda.interval <- c(0.2, 0.2)
  mu.interval     <- c(0.15, 0.15)
  nu.interval     <- seq(from = 1, to = 2.5, by = 0.5)
  q.interval      <- seq(from = 0.10, to = 0.20, by = 0.05)
  seed.interval   <- 1:3
  soc  <- 2
  age  <- 10
  cond <- 1
  sequence_length <- 10^3
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

  parameters_filename <- filenames[1]

  # Work on the parameter file
  input_filenames <- razzo::raz_create_input_files(parameters = parameters, folder_name = folder_name)
  testthat::expect_true(
    length(unique(dirname(input_filenames))) == 1
  )
  directory_name <- unique(dirname(input_filenames))

  # Expect four files to be created
  mbd_fasta_filename <- file.path(directory_name, "mbd.fasta")
  mbd_tree_filename  <- file.path(directory_name, "mbd.tree")
  bd_fasta_filename  <- file.path(directory_name, "bd.fasta")
  bd_tree_filename   <- file.path(directory_name, "bd.tree")

  testthat::expect_true(file.exists(mbd_tree_filename))
  # these files are not created by raz_create_input_files
  if (1 == 2) {
    testthat::expect_true(file.exists(mbd_fasta_filename))
    testthat::expect_true(file.exists(bd_tree_filename))
    testthat::expect_true(file.exists(bd_fasta_filename))
  }

})
