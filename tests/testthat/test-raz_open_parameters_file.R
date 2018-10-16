context("raz_open_parameters_file")

test_that("use", {

  # Work from a folder
  folder_name <- raz_tempdir(); # folder_name <- tempdir()
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
  filenames <- raz_create_parameters_files(folder_name = folder_name,
                                           lambda.interval = lambda.interval,
                                           mu.interval = mu.interval,
                                           nu.interval = nu.interval,
                                           q.interval = q.interval,
                                           seed.interval = seed.interval,
                                           soc = soc,
                                           age = age,
                                           cond = cond)

  filename <- filenames[1]
  parameters <- raz_open_parameters_file(filename)
  testthat::expect_true(parameters$lambda > 0.0)
  testthat::expect_true(parameters$mu >= 0.0)
  testthat::expect_true(parameters$nu >= 0.0)
  testthat::expect_true(parameters$q >= 0.0 && parameters$q <= 1.0)
})
