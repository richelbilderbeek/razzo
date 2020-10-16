test_that("use", {

  df <- razzo::collect_all_data(
    project_folder_name = dirname(dirname(system.file("extdata", "mbd.fasta", package = "razzo"))), # nolint long path indeed, fine for test
    # project_folder_name = raztr::get_raztr_path("razzo_project"), # nolint
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
  expect_true("folder" %in% names(df))
  expect_true("tree" %in% names(df))
  expect_true("best_or_gen" %in% names(df))
  expect_true("ess_likelihood" %in% names(df))
  expect_true("lambda" %in% names(df))
  expect_true("mu" %in% names(df))
  expect_true("nu" %in% names(df))
  expect_true("q" %in% names(df))
  expect_true("cond" %in% names(df)) # Currently always 1
  expect_true("crown_age" %in% names(df))
  expect_true("seed" %in% names(df))
  expect_true("n_mb_species" %in% names(df))
  expect_true("f_mb_species" %in% names(df))
  expect_true("n_mutations" %in% names(df))
  expect_true("n_taxa" %in% names(df))
  expect_true("nltt_1" %in% names(df))
  expect_true("nltt_2" %in% names(df))

  # Data must make sense
  expect_true(all(df$tree %in% c("true", "twin")))
  expect_true(all(df$best_or_gen %in% c("best", "gen")))
  expect_true(all(df$lambda >= 0.0))
  expect_true(all(df$mu >= 0.0))
  expect_true(all(df$nu >= 0.0))
  expect_true(all(df$q >= 0.0 & df$q <= 1.0))
  expect_true(all(df$crown_age >= 0.0))
  expect_true(all(df$seed >= 0))
  expect_true(all(df$n_mb_species >= 0))
  expect_true(all(df$f_mb_species >= 0))
  expect_true(all(df$f_mb_species <= 1))
  expect_true(all(df$n_mutations >= 0))
  expect_true(all(df$n_taxa >= 0))
  expect_true(all(df$nltt_1 >= 0))
})

test_that("abuse", {

  expect_error(
    razzo::collect_all_data(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
