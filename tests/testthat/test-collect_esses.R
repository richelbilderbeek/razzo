test_that("use", {
  # Works locally
  if (beastier::is_on_travis()) return()

  df <- razzo::collect_esses(
    project_folder_name = raztr::get_raztr_path("razzo_project")
  )

  # Sub-keys
  testthat::expect_true("tree" %in% names(df))
  testthat::expect_true("best_or_gen" %in% names(df))

  # Only take the ESS of the likelihood
  testthat::expect_true("ess_likelihood" %in% names(df))
  testthat::expect_true(all(df$ess_likelihood <= 8))

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

  # raztr has run one setting ..."
  #  - with two seeds
  #  - for true and twin tree
  #  - for generative and candidate
  # This is 2x2x2 Effective Sample Sizes
  testthat::expect_equal(nrow(df), 8)
})

test_that("abuse", {
  testthat::expect_error(
    razzo::collect_esses(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project' or 'raket_werper'"
  )
})
