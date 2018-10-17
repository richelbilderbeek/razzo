context("raz_create_bd_alignment")

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

  # parameters_filename   <- file.path(folder_name, sub_folder_name, "parameters.csv")
  # bd_alignment_filename <- file.path(folder_name, sub_folder_name, "bd.fasta")

  # TODO: Issue #15: Add 'raz_create_bd_alignment'
  if (1 == 2) {
    raz_create_bd_alignment(parameters_filename)
    expect_true(file.exists(bd_alignment_filename))
  }
})
