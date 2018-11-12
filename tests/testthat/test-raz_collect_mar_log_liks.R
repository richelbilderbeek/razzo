context("test-raz_collect_mar_log_liks")

test_that("use", {
  skip("TODO. Issue 77, #77")

  df <- raz_collect_mar_log_liks(raz_get_path("razzo_project"))

  # Experimental parameters that vary
  expect_true("lambda" %in% names(df))
  expect_true("mu" %in% names(df))
  expect_true("nu" %in% names(df))
  expect_true("q" %in% names(df))
  expect_true("seed" %in% names(df))
  expect_true("site_model" %in% names(df))
  expect_true("clock_model" %in% names(df))

  # The collected marginal log-likelihood
  expect_true("mar_log_lik" %in% names(df))

  # Data must be tidy
  expect_true(is.factor(df$clock_model))
  expect_true(is.factor(df$site_model))

})

test_that("abuse", {
  skip("TODO. Issue 77, #77")
  expect_error(
    raz_collect_mar_log_liks("nonsense"),
    "'folder_name' must end with 'razzo_project'"
  )
})
