test_that("matches article", {

  gen_exp <- create_razzo_gen_experiment()
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
    6.0
  )
  expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$sigma$value,
    0.0001
  )
  expect_equal(gen_exp$inference_model$mcmc$chain_length, 1e6)
  expect_equal(gen_exp$inference_model$mcmc$store_every, 1e3)
  expect_equal(gen_exp$est_evidence_mcmc$chain_length, 1e6)
  # gen_exp$est_evidence_mcmc
  expect_equal(
    gen_exp$est_evidence_mcmc$chain_length,
    create_razzo_nested_sampling_mcmc()$chain_length
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$store_every,
    create_razzo_nested_sampling_mcmc()$store_every
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$epsilon,
    create_razzo_nested_sampling_mcmc()$epsilon
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$particle_count,
    create_razzo_nested_sampling_mcmc()$particle_count
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$sub_chain_length,
    create_razzo_nested_sampling_mcmc()$sub_chain_length
  )
  # --------------------
  # beast2_options
  # --------------------
  # gen_exp$beast2_options$rng_seed must match MBD RNG seed
  # --------------------
  # est_evidence_mcmc
  # --------------------
  gen_exp$est_evidence_mcmc
})
