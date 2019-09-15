test_that("use", {
  df <- measure_taxa_and_mbness(n_replicates = 1)

  # Column names
  expect_true("lambda" %in% names(df))
  expect_true("mu" %in% names(df))
  expect_true("nu" %in% names(df))
  expect_true("q" %in% names(df))
  expect_true("crown_age" %in% names(df))
  expect_true("cond" %in% names(df))
  expect_true("seed" %in% names(df))
  expect_true("n_taxa" %in% names(df))
  expect_true("precentage_mb_species" %in% names(df))
  expect_true("setting" %in% names(df))

  # Values
  expect_true(all(df$lambda >= 0.0))
  expect_true(all(df$mu >= 0.0))
  expect_true(all(df$nu >= 0.0))
  expect_true(all(df$q >= 0.0))
  expect_true(all(df$crown_age >= 0.0))
  expect_true(all(df$cond == 1))
  expect_true(all(df$n_taxas >= 0.0))
  expect_true(all(df$precentage_mb_species >= 0.0))
  expect_true(all(df$precentage_mb_species <= 100.0))
})
