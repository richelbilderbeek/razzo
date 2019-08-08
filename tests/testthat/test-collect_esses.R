context("test-collect_esses")

test_that("use", {

  # Error in collect_esses(project_folder_name = get_razzo_path("razzo_project")) :
  # No .log files found at path '/home/richel/GitHubs/razzo/inst/extdata/razzo_project/data/0.2-0.15-1-0.1/1'
  # Maybe the razzo experiment is not run yet?
  if (1 == 2) {
    for (filename in list.files(
        get_razzo_path("razzo_project"),
        recursive = TRUE, pattern = "parameters\\.RDa",
        full.names = TRUE
      )
    ) {
      beautier::check_file_exists(filename)
      run_razzo(open_parameters_file(filename))
    }
  }
  df <- collect_esses(
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

  # Only take the ESS of the likelihood
  expect_true("ess_likelihood" %in% names(df))

  # Unsure, with the bug it is 4, so hope to get it lower after fixing it
  expect_true(all(df$ess_likelihood <= 4))

  # Data must be tidy
  expect_true(is.factor(df$clock_model))
  expect_true(is.factor(df$site_model))

  # Rows must be unique
  expect_equal(nrow(unique(df)), nrow(df))
})

test_that("abuse", {
  expect_error(
    collect_esses(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
