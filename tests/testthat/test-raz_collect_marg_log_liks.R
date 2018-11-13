context("raz_collect_marg_log_liks")

test_that("use", {

  df <- raz_collect_marg_log_liks(
    project_folder_name = raz_get_path("razzo_project")
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
  expect_true(all(df$gen_model %in% raz_gen_models())) # nolint internal function
  expect_true(all(df$site_model %in% raz_site_models())) # nolint internal function
  expect_true(all(df$close_model %in% raz_clock_models())) # nolint internal function
  expect_true(all(df$marg_log_lik <= 0))
  expect_true(all(df$marg_log_lik_sd >= 0))
})

test_that("abuse", {
  skip("TODO. Issue 77, #77")
  expect_error(
    raz_collect_mar_log_liks(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
