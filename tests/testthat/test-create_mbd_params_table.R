test_that("every row must be unique", {
  df <- create_mbd_params_table()
  expect_true(is.data.frame(df))
  expect_equal(nrow(unique(df)), nrow(df))
  expect_true("lambda" %in% names(df))
  expect_true("mu" %in% names(df))
  expect_true("nu" %in% names(df))
  expect_true("q" %in% names(df))
  expect_true("crown_age" %in% names(df))
  expect_true("seed" %in% names(df))
})

test_that("can plug in mbd.TaxaAndMbness::create_params_table", {
  df <- mbd.TaxaAndMbness::create_params_table()
  expect_true(is.data.frame(df))
  expect_equal(nrow(unique(df)), nrow(df))
  expect_true("lambda" %in% names(df))
  expect_true("mu" %in% names(df))
  expect_true("nu" %in% names(df))
  expect_true("q" %in% names(df))
  expect_true("crown_age" %in% names(df))
  expect_true("seed" %in% names(df))
})

test_that("matches article", {
  skip("No need to match article for exploration in #336")
  # Issue 242, Isssue #242
  n_replicates <- 1
  mbd_params_table <- create_mbd_params_table(
    n_replicates = n_replicates
  )
  n_mbd_params <- nrow(mbd_params_table)

  expected_lambdas <- c(0.2)
  expect_true(all(mbd_params_table$lambda %in% expected_lambdas))
  expect_true(all(expected_lambdas %in% mbd_params_table$lambda))

  expected_mus <- c(0.0, 0.15)
  expect_true(all(mbd_params_table$mu %in% expected_mus))
  expect_true(all(expected_mus %in% mbd_params_table$mu))

  expected_nus <- get_razzo_nus()
  expect_true(all(mbd_params_table$nu %in% expected_nus))
  expect_true(all(expected_nus %in% mbd_params_table$nu))

  expected_qs <- c(0.10, 0.15, 0.20)
  expect_true(all(mbd_params_table$q %in% expected_qs))
  expect_true(all(expected_qs %in% mbd_params_table$q))

  expected_crown_age <- get_razzo_crown_age()
  expect_true(all(mbd_params_table$crown_age == expected_crown_age))

  expected_cond <- 1
  expect_true(all(expected_cond == mbd_params_table$cond))

  expect_true(all(unique(mbd_params_table$seed) == mbd_params_table$seed))
})
