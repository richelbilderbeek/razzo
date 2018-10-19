context("raz_create_parameters_files")

test_that("use", {
  # gio: I don't really get how to use this

  # # Will be something like 'home/tece/razzo_project/data
  # razzo_project_data_folder <- file.path(rappdirs::user_data_dir(), "razzo_project", "data")
  # filenames <- raz_create_parameters_files(folder = razzo_project_data_folder)
  # expect_true(length(filenames) >= 1)
  # # TODO: add test that foldername is in filename, e.g. 'myfolder/1.csv'
  # expect_true(all(file.exists(filenames)))

  # Work from a folder
  folder_name <- razzo:::raz_make_tempdir(); # folder_name <- tempdir()
  testthat::expect_true(
    dir.exists(folder_name)
  )

  # Create the parameter files
  razzo:::raz_save_standard_test_parameters()
  filenames <- razzo:::raz_get_standard_test_filenames()

  testthat::expect_true(length(filenames) >= 1)
  testthat::expect_true(all(file.exists(filenames)))

})
