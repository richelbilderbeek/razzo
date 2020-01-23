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
    get_razzo_mcmc_chain_length()
  )
  expect_equal(
    gen_experiment$inference_model$mcmc$tracelog$log_every,
    get_razzo_mcmc_store_every()
  )
  expect_equal(
    gen_experiment$inference_model$mcmc$treelog$log_every,
    get_razzo_mcmc_store_every()
  )

  if (rappdirs::app_dir()$os == "win") return()

  folder_name <- peregrine::get_pff_tempfile()

  cand_experiments <- razzo::create_razzo_cand_experiments(
    gen_experiment = gen_experiment,
    folder_name = folder_name,
    rng_seed = 1
  )
  for (cand_experiment in cand_experiments) {
    expect_equal(
      cand_experiment$inference_model$mcmc$chain_length,
      razzo::get_razzo_mcmc_chain_length()
    )
  }

  for (i in seq_along(cand_experiments)) {

    expect_equal(
      cand_experiments[[i]]$est_evidence_mcmc$tracelog$filename,
      file.path(folder_name, paste0("mbd_best_", i, "_evidence.log"))
    )
    expect_equal(
      cand_experiments[[i]]$est_evidence_mcmc$treelog$filename,
      file.path(folder_name, paste0("mbd_best_", i, "_evidence.trees"))
    )
  }
})
