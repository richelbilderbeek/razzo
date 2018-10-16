context("raz_create_parameters_files")

test_that("use", {
  # Will be something like 'home/tece/razzo_project/data
  razzo_project_data_folder <- file.path(rappdirs::user_data_dir(), "razzo_project", "data")
  filenames <- raz_create_parameters_files(folder = razzo_project_data_folder)
  expect_true(length(filenames) >= 1)
  # TODO: add test that foldername is in filename, e.g. 'myfolder/1.csv'
  expect_true(all(file.exists(filenames)))
})
