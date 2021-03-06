context("collect_mbd_params")

test_that("use", {

  df <- razzo::collect_mbd_params(
    project_folder_name = raztr::get_raztr_path("razzo_project")
  )

  # Use relative folder path as the primary key
  #
  # E.g. an experiment with its parameter file at either of these locations
  #
  #   /home/richel/razzo_project/data/1/2/3/parameters.RDa
  #   C:\Giappo\razzo_project\data\1\2\3\parameters.RDa
  #
  # would get
  #
  #  data/1/2/3                                                                 # nolint this is not commented code
  #
  testthat::expect_true("folder" %in% names(df))

  # The measurements, simply all arguments of 'create_params_mbd'
  testthat::expect_true("lambda" %in% names(df))
  testthat::expect_true("mu" %in% names(df))
  testthat::expect_true("nu" %in% names(df))
  testthat::expect_true("q" %in% names(df))
  testthat::expect_true("cond" %in% names(df)) # Currently always 1
  testthat::expect_true("crown_age" %in% names(df))
  testthat::expect_true("seed" %in% names(df))

  # Data must make sense
  testthat::expect_true(all(df$lambda >= 0.0))
  testthat::expect_true(all(df$mu >= 0.0))
  testthat::expect_true(all(df$nu >= 0.0))
  testthat::expect_true(all(df$q >= 0.0 & df$q <= 1.0))
  testthat::expect_true(all(df$crown_age >= 0.0))
})

test_that("abuse", {

  testthat::expect_error(
    razzo::collect_mbd_params(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
