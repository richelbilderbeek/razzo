test_that("must create experiments of the razzo MCMC chain length", {

  gen_experiment <- pirouette::create_gen_experiment(
    inference_model = beautier::create_inference_model(
      mcmc = get_razzo_mcmc(
        model_type = "generative"
      )
    )
  )
  expect_equal(
    gen_experiment$inference_model$mcmc$chain_length,
    razzo::get_razzo_mcmc_chain_length()
  )

  if (rappdirs::app_dir()$os == "win") return()

  cand_experiments <- razzo::create_razzo_cand_experiments(
    gen_experiment = gen_experiment,
    folder_name = peregrine::get_pff_tempfile(),
    rng_seed = 1
  )
  for (cand_experiment in cand_experiments) {
    expect_equal(
      cand_experiment$inference_model$mcmc$chain_length,
      razzo::get_razzo_mcmc_chain_length()
    )
  }
})
