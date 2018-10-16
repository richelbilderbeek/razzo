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
  soc <- 2
  age <- 10
  cond <- 1
  folder_name <- tempdir()
  filenames <- raz_create_parameters_files(folder_name = folder_name,
                                           lambda.interval = c(0.2, 0.2),
                                           mu.interval = c(0.15, 0.15),
                                           nu.interval = seq(from = 1, to = 2.5, by = 0.5),
                                           q.interval = seq(from = 0.10, to = 0.20, by = 0.05),
                                           seed.interval = 1:3,
                                           soc = soc,
                                           age = age,
                                           cond = cond)
  testthat::expect_true(length(filenames) >= 1)

  # TODO: Issue #2
  if (1 == 2) {
    expect_true(all(file.exists(filenames)))
  }
})
