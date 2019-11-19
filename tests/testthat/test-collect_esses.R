context("test-collect_esses")

test_that("use", {

  # Error in collect_esses(project_folder_name = get_razzo_path("razzo_project")) :  # nolint error message is indeed long
  # No .log files found at path
  # '/home/richel/GitHubs/razzo/inst/extdata/razzo_project/data/0.2-0.15-1-0.1/1' # nolint path is indeed long
  # Maybe the razzo experiment is not run yet?
  if (1 == 2) {
    for (filename in list.files(
      razzo::get_razzo_path("razzo_project"),
      recursive = TRUE, pattern = "parameters\\.RDa",
      full.names = TRUE
    )
    ) {
      beautier::check_file_exists(filename)
      razzo::run_razzo(razzo::open_parameters_file(filename))
    }
  }
  df <- razzo::collect_esses(
    project_folder_name = razzo::get_razzo_path("razzo_project")
  )

  # Sub-keys
  testthat::expect_true("tree" %in% names(df))
  testthat::expect_true("best_or_gen" %in% names(df))

  # Only take the ESS of the likelihood
  testthat::expect_true("ess_likelihood" %in% names(df))

  # Unsure, with the bug it is 4, so hope to get it lower after fixing it
  testthat::expect_true(all(df$ess_likelihood <= 4))

  # Data must be tidy
  testthat::expect_true(is.factor(df$tree))
  testthat::expect_true(is.factor(df$best_or_gen))

  # Rows must be unique
  testthat::expect_equal(nrow(unique(df)), nrow(df))

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

  # Removed obsolete values, Issue #253
  testthat::expect_false("site_model" %in% names(df))
  testthat::expect_false("clock_model" %in% names(df))
  testthat::expect_false("tree_prior" %in% names(df))
  testthat::expect_false("burn_in_fraction" %in% names(df))
  testthat::expect_false("sample_interval" %in% names(df))
})

test_that("abuse", {
  testthat::expect_error(
    razzo::collect_esses(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project' or 'raket_werper'"
  )
})
