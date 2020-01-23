test_that("matches article", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  folder_name <- peregrine::get_pff_tempfile()

  gen_exp <- create_razzo_gen_experiment(
    folder_name = folder_name
  )
  # --------------------
  # Inference conditions
  # --------------------
  expect_equal(gen_exp$inference_conditions$model_type, "generative")
  expect_equal(gen_exp$inference_conditions$run_if, "always")
  expect_equal(gen_exp$inference_conditions$do_measure_evidence, TRUE)
  # --------------------
  # Inference model
  # --------------------
  expect_equal(gen_exp$inference_model$site_model$name, "JC69")
  expect_equal(gen_exp$inference_model$clock_model$name, "strict")
  expect_equal(gen_exp$inference_model$tree_prior$name, "birth_death")
  expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$name,
    "normal"
  )
  expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$mean$value,
    get_razzo_crown_age()
  )
  expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$sigma$value,
    0.0001
  )
  expect_equal(
    gen_exp$inference_model$mcmc$chain_length,
    get_razzo_mcmc_chain_length()
  )
  expect_equal(
    gen_exp$inference_model$mcmc$store_every,
    get_razzo_mcmc_store_every()
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$chain_length,
    get_razzo_mcmc_chain_length()
  )
  # gen_exp$est_evidence_mcmc
  ns_mcmc_gen <- razzo::create_razzo_ns_mcmc(
    folder_name = folder_name,
    model_type = "generative"
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$chain_length,
    ns_mcmc_gen$chain_length
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$store_every,
    ns_mcmc_gen$store_every
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$epsilon,
    ns_mcmc_gen$epsilon
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$particle_count,
    ns_mcmc_gen$particle_count
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$sub_chain_length,
    ns_mcmc_gen$sub_chain_length
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$tracelog$filename,
    ns_mcmc_gen$tracelog$filename
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$tracelog$log_every,
    ns_mcmc_gen$tracelog$log_every
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$treelog$filename,
    ns_mcmc_gen$treelog$filename
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$treelog$log_every,
    ns_mcmc_gen$treelog$log_every
  )
  skip("TODO")
  # gen_exp$beast2_options$rng_seed must match MBD RNG seed
})

test_that("razzo naming scheme", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  folder_name <- peregrine::get_pff_tempfile()
  gen_exp <- create_razzo_gen_experiment(folder_name = folder_name)

  # gen_exp$inference_model
  expect_equal(
    gen_exp$inference_model$mcmc$tracelog$filename,
    file.path(folder_name, "mbd_gen.log")
  )
  expect_equal(
    gen_exp$inference_model$mcmc$treelog$filename,
    file.path(folder_name, "mbd_gen.trees")
  )
  # gen_exp$beast2_options
  expect_equal(
    gen_exp$beast2_options$input_filename,
    file.path(folder_name, "mbd_gen.xml")
  )
  expect_equal(
    gen_exp$beast2_options$output_state_filename,
    file.path(folder_name, "mbd_gen.xml.state")
  )

  # gen_exp$errors_filename
  expect_equal(
    gen_exp$errors_filename,
    file.path(folder_name, "mbd_nltts_gen.csv")
  )

  # est_evidence_mcmc
  expect_equal(
    gen_exp$est_evidence_mcmc$treelog$filename,
    file.path(folder_name, "mbd_gen_evidence.trees")
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$tracelog$filename,
    file.path(folder_name, "mbd_gen_evidence.log")
  )
})
