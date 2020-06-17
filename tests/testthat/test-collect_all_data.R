context("collect_all_data")

test_that("use", {

  df <- razzo::collect_all_data(
    project_folder_name = raztr::get_raztr_path("razzo_project"),
    new_run = TRUE
  )

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
  testthat::expect_true("tree" %in% names(df))
  testthat::expect_true("best_or_gen" %in% names(df))
  testthat::expect_true("ess_likelihood" %in% names(df))
  testthat::expect_true("lambda" %in% names(df))
  testthat::expect_true("mu" %in% names(df))
  testthat::expect_true("nu" %in% names(df))
  testthat::expect_true("q" %in% names(df))
  testthat::expect_true("cond" %in% names(df)) # Currently always 1
  testthat::expect_true("crown_age" %in% names(df))
  testthat::expect_true("seed" %in% names(df))
  testthat::expect_true("n_mb_species" %in% names(df))
  testthat::expect_true("f_mb_species" %in% names(df))
  testthat::expect_true("n_mutations" %in% names(df))
  testthat::expect_true("n_taxa" %in% names(df))
  testthat::expect_true("nltt_1" %in% names(df))
  testthat::expect_true("nltt_2" %in% names(df))

  # Data must make sense
  testthat::expect_true(all(df$tree %in% c("true", "twin")))
  testthat::expect_true(all(df$best_or_gen %in% c("best", "gen")))
  testthat::expect_true(all(df$lambda >= 0.0))
  testthat::expect_true(all(df$mu >= 0.0))
  testthat::expect_true(all(df$nu >= 0.0))
  testthat::expect_true(all(df$q >= 0.0 & df$q <= 1.0))
  testthat::expect_true(all(df$crown_age >= 0.0))
  testthat::expect_true(all(df$seed >= 0))
  testthat::expect_true(all(df$n_mb_species >= 0))
  testthat::expect_true(all(df$f_mb_species >= 0))
  testthat::expect_true(all(df$f_mb_species <= 1))
  testthat::expect_true(all(df$n_mutations >= 0))
  testthat::expect_true(all(df$n_taxa >= 0))
  testthat::expect_true(all(df$nltt_1 >= 0))
})

test_that("abuse", {

  testthat::expect_error(
    razzo::collect_all_data(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
