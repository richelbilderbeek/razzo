test_that("use", {
  gen_experiment <- create_gen_experiment()
  expect_equal(
    gen_experiment$inference_model$mcmc$chain_length,
    get_razzo_mcmc_chain_length()
  )
  cand_experiments <- create_razzo_cand_experiments(
    gen_experiment = gen_experiment,
    folder_name = peregrine::get_pff_tempfile(),
    rng_seed = 1
  )
  skip("Expose #350")
  expect_equal(
    cand_experiments[[1]]$inference_model$mcmc$chain_length,
    get_razzo_mcmc_chain_length()
  )
})
