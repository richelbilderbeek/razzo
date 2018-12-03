context("collect_marg_liks")

test_that("use", {

  df <- collect_marg_liks(
    project_folder_name = get_path("razzo_project")
  )

  # Experimental parameters that vary
  expect_true("lambda" %in% names(df))
  expect_true("mu" %in% names(df))
  expect_true("nu" %in% names(df))
  expect_true("q" %in% names(df))
  expect_true("seed" %in% names(df))
  expect_true("site_model" %in% names(df))
  expect_true("clock_model" %in% names(df))

  # gen_model is the generative model,
  # can be 'mbd' (the MBD tree)
  # or 'bd' (for the twin BD tree)
  expect_true("gen_model" %in% names(df))

  # The collected marginal log-likelihood
  expect_true("marg_log_lik" %in% names(df))

  # Data must be tidy
  expect_true(is.factor(df$gen_model))
  expect_true(is.factor(df$clock_model))
  expect_true(is.factor(df$site_model))

  # Data must make sense
  expect_true(all(df$lambda >= 0))
  expect_true(all(df$mu >= 0))
  expect_true(all(df$nu >= 0))
  expect_true(all(df$q >= 0 & df$q <= 1))
  expect_true(all(df$crown_age >= 0))
  expect_true(all(df$sequence_length >= 0))
  expect_true(all(df$sample_interval >= 0))
  expect_true(all(df$chain_length >= 0))
  expect_true(all(df$sub_chain_length >= 0))
  expect_true(all(df$gen_model %in% get_gen_models())) # nolint internal function
  expect_true(all(df$site_model %in% get_site_models())) # nolint internal function
  expect_true(all(df$close_model %in% get_clock_models())) # nolint internal function
  expect_true(all(df$marg_log_lik <= 0))
  expect_true(all(df$marg_log_lik_sd >= 0))
  expect_true(df$marg_log_lik[1] != df$marg_log_lik[2])
})

test_that("abuse", {
  expect_error(
    collect_marg_liks(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
