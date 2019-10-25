test_that("must create experiments of the razzo MCMC chain length", {

  gen_experiment <- create_gen_experiment(
    inference_model = beautier::create_inference_model(
      mcmc = get_razzo_mcmc()
    )
  )
  expect_equal(
    gen_experiment$inference_model$mcmc$chain_length,
    get_razzo_mcmc_chain_length()
  )
  cand_experiments <- create_razzo_cand_experiments(
    gen_experiment = gen_experiment,
    folder_name = peregrine::get_pff_tempfile(),
    rng_seed = 1
  )
  for (cand_experiment in cand_experiments) {
    expect_equal(
      cand_experiment$inference_model$mcmc$chain_length,
      get_razzo_mcmc_chain_length()
    )
  }
})