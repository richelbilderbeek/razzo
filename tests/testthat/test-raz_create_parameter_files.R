context("raz_create_parameters_files")

test_that("use", {
#<<<<<<< HEAD
  # gio: I don't really get how to use this
  # # Will be something like 'home/tece/razzo_project/data
  # razzo_project_data_folder <- file.path(rappdirs::user_data_dir(), "razzo_project", "data")
  # filenames <- raz_create_parameters_files(folder = razzo_project_data_folder)
  # expect_true(length(filenames) >= 1)
  # # TODO: add test that foldername is in filename, e.g. 'myfolder/1.csv'
  # expect_true(all(file.exists(filenames)))
#=======
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
  soc <- 2
  age <- 10
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
  testthat::expect_true(length(filenames) >= 1)
  testthat::expect_true(all(file.exists(filenames)))

})
