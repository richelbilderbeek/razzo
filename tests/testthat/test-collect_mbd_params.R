test_that("use", {

  skip("Issue 230, Issue #230")

  df <- collect_mbd_params(
    project_folder_name = get_razzo_path("razzo_project")
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
  #  data/1/2/3
  #
  expect_true("folder" %in% names(df))

  # The measurements, simply all arguments of 'create_params_mbd'
  expect_true("lambda" %in% names(df))
  expect_true("mu" %in% names(df))
  expect_true("nu" %in% names(df))
  expect_true("q" %in% names(df))
  expect_true("cond" %in% names(df)) # Currently always 1 (whatever that means)
  expect_true("crown_age" %in% names(df))
  expect_true("seed" %in% names(df))

  # Data must make sense
  expect_true(all(df$lambda >= 0.0))
  expect_true(all(df$mu >= 0.0))
  expect_true(all(df$nu >= 0.0))
  expect_true(all(df$q >= 0.0 & df$q <= 1.0))
  expect_true(all(df$crown_age >= 0.0))
})

test_that("abuse", {
  expect_error(
    collect_mbd_params(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
