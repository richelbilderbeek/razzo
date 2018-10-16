context("raz_open_parameters_file")

test_that("use", {

  folder_name <- tempdir()
  soc <- 2
  age <- 10
  cond <- 1
  filenames <- raz_create_parameters_files(folder_name = folder_name,
                                           lambda.interval = c(0.2, 0.2),
                                           mu.interval = c(0.15, 0.15),
                                           nu.interval = seq(from = 1, to = 2.5, by = 0.5),
                                           q.interval = seq(from = 0.10, to = 0.20, by = 0.05),
                                           seed.interval = 1:3,
                                           soc = soc,
                                           age = age,
                                           cond = cond)
  # filenames <- raz_create_parameters_files(folder_name = folder_name)
  filename <- filenames[1]
  parameters <- raz_open_parameters_file(filename)
  testthat::expect_true(parameters$lambda > 0.0)
})
