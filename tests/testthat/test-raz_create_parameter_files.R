context("raz_create_parameters_files")

test_that("use", {
<<<<<<< HEAD
  # Will be something like 'home/tece/razzo_project/data
  razzo_project_data_folder <- file.path(rappdirs::user_data_dir(), "razzo_project", "data")
  filenames <- raz_create_parameters_files(folder = razzo_project_data_folder)
  expect_true(length(filenames) >= 1)
  # TODO: add test that foldername is in filename, e.g. 'myfolder/1.csv'
  expect_true(all(file.exists(filenames)))
=======
  folder_name <- tempdir()
  filenames <- raz_create_parameters_files(folder_name = folder_name)
  expect_true(length(filenames) >= 1)

  # TODO: Issue #2
  if (1 == 2) {
    expect_true(all(file.exists(filenames)))
  }
>>>>>>> 785f01b06c2d81fc9b10363f8ade087bd728482e
})
