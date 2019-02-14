context("test-run_razzo")

test_that("use", {

  if (!beastier::is_on_travis()) return()

  razzo_params <- create_test_razzo_params()
  run_razzo(razzo_params)
})
