context("test-raz_collect_esses")

test_that("use", {
  skip("TODO. Issue 78, #78")

  df <- raz_collect_esses(
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

  # The collected effective sample sizes

  if (1 == 2) {
    # Here one can see the ESSes from one file
    example_log_file <- raz_get_path(
      "razzo_project/data/0.2-0.15-1-0.1/1/rln-gtr/mbd.log"
    )
    examples_esses <- tracerer::calc_esses(
      tracerer::parse_beast_log(example_log_file), sample_interval = 1000
    )
  }
  # I (@richelbilderbeek) suggest to take the ESS of the likelihood,
  # and that one only. We'll see if that idea changes in the future.
  expect_true("ess_likelihood" %in% names(df))

  # Data must be tidy
  expect_true(is.factor(df$gen_model))
  expect_true(is.factor(df$clock_model))
  expect_true(is.factor(df$site_model))

})

test_that("abuse", {
  skip("TODO. Issue 78, #78")
  expect_error(
    raz_collect_esses(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
