context("test-run_razzo")

test_that("use", {

  if (!beastier::is_on_travis()) return()

  skip("Expose #350")

  razzo_params <- create_test_razzo_params()
  run_razzo(razzo_params)

  # Check if MCMC chain length in parameters matches the final state
  # in the BEAST2 output files
  expected_mcmc_chain_length <-
    razzo_params$pir_params$experiments[[1]]$inference_model$mcmc$chain_length

  final_mcmc_state_line <- tail(
    readLines(
      razzo_params$pir_params$experiments[[1]]$inference_model$mcmc$treelog$filename, # nolint sorry Demeter
      warn = FALSE
    ),
    n = 2
  )[1]
  final_mcmc_state_line
  actual_mcmc_chain_length <- as.numeric(
    stringr::str_match(
      string = final_mcmc_state_line,
      pattern = "tree STATE_(.*) = \\("
    )[1, 2]
  )
  expect_equal(
    expected_mcmc_chain_length,
    actual_mcmc_chain_length
  )
})
