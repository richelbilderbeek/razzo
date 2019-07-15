test_that("use", {

  if (1 == 2) {
    # Run the experiment if you can and need to
    for (file in list.files(get_razzo_path("razzo_project"), recursive = TRUE, pattern = "parameters\\.RDa")) {
      run_razzo(open_parameters_file(file))
    }
  }
  df <- collect_n_taxa(
    project_folder_name = get_razzo_path("razzo_project")
  )

  # Experimental parameters that vary
  expect_true("lambda" %in% names(df))
  expect_true("mu" %in% names(df))
  expect_true("nu" %in% names(df))
  expect_true("q" %in% names(df))
  expect_true("seed" %in% names(df))
  expect_true("site_model" %in% names(df))
  expect_true("clock_model" %in% names(df))
  expect_true("tree" %in% names(df))
  # The number of taxa
  expect_true("n_taxa" %in% names(df))

  # Data must be tidy
  expect_true(is.factor(df$clock_model))
  expect_true(is.factor(df$site_model))

  # Rows must be unique
  expect_equal(nrow(unique(df)), nrow(df))

  # Data must make sense
  expect_true(is.numeric(df$n_taxa)) # n_taxa is numeric
  df_list <- split(
    df,
    f = list(
      df$lambda,
      df$mu,
      df$nu,
      df$q,
      df$seed,
      df$crown_age,
      df$cond,
      df$site_model,
      df$clock_model
    )
  )
  skip("Issue to be solved #210")
  expect_true( # same n_taxa for true and twin
    all(
      unlist(lapply(df_list, FUN = function(x) length(unique(x$n_taxa)) == 1))
    )
  )

})
