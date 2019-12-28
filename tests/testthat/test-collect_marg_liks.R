context("test-collect_marg_liks")

test_that("use", {

  df <- razzo::collect_marg_liks(
    project_folder_name = razzo::get_razzo_path("razzo_project")
  )

  # Secondary keys
  expect_true("tree" %in% names(df))
  expect_true("site_model" %in% names(df))
  expect_true("clock_model" %in% names(df))
  expect_true("tree_prior" %in% names(df))

  # Measurements
  expect_true("marg_log_lik" %in% names(df))
  expect_true("marg_log_lik_sd" %in% names(df))
  expect_true("weight" %in% names(df))

  # Data must be tidy
  expect_true(is.factor(df$tree))
  expect_true(is.factor(df$clock_model))
  expect_true(is.factor(df$site_model))
  expect_true(is.factor(df$tree_prior))

  # Data must make sense
  expect_true(
    all(df$site_model %in% beautier::get_site_model_names())
  )
  expect_true(
    all(df$clock_model %in% beautier::get_clock_model_names())
  )
  expect_true(all(df$marg_log_lik <= 0))
  expect_true(all(df$marg_log_lik_sd >= 0))

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
  expect_true("folder" %in% names(df))
})

test_that("abuse", {
  expect_error(
    razzo::collect_marg_liks(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project' or 'raket_werper'"
  )
})
