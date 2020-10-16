test_that("use", {
  # Works locally
  if (beastier::is_on_travis()) return()

  df <- collect_nltt_stats(
    project_folder_name = raztr::get_raztr_path("razzo_project")
  )

  # Secondary keys
  # tree can be can be 'true' (the MBD tree)
  # or 'twin' (for the twin BD tree)
  expect_true("tree" %in% names(df))
  expect_true("best_or_gen" %in% names(df))

  # The measurementss
  # The collected nLTT statistics, start counting from 1
  expect_true("nltt_1" %in% names(df))
  expect_true("nltt_2" %in% names(df))

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

  # Data must be tidy
  expect_true(is.factor(df$tree))
  expect_true(is.factor(df$best_or_gen))


  # Data must make sense
  expect_true(all(df$best_or_gen %in% c("best", "gen")))
  expect_true(all(df$nltt_1 >= 0))

  # Obsolete values
  expect_false("site_model" %in% names(df))
  expect_false("clock_model" %in% names(df))
  expect_false("tree_prior" %in% names(df))
})

test_that("abuse", {
  expect_error(
    collect_nltt_stats(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
