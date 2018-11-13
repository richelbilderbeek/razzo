context("test-raz_collect_nltt_stats")

test_that("use", {
  skip("TODO. Issue 84, #84")

  df <- raz_collect_nltt_stats(
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

  # The collected nLTT statistics, start counting from 1
  expect_true("nltt_1" %in% names(df))
  expect_true("nltt_2" %in% names(df))

  # Data must be tidy
  expect_true(is.factor(df$gen_model))
  expect_true(is.factor(df$clock_model))
  expect_true(is.factor(df$site_model))
})

test_that("abuse", {
  skip("TODO. Issue 84, #84")
  expect_error(
    raz_collect_nltt_stats(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
