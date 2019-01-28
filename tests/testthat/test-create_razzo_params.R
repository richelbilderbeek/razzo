context("create_razzo_params")

test_that("use", {

  crown_age <- 15
  crown_age_sigma <- 0.01
  mbd_params <- becosys::create_mbd_params(
    lambda = 0.1,
    mu = 0.15,
    nu = 0.2,
    q = 0.01
  )
  twinning_params <- create_twinning_params(rng_seed = 123)
  alignment_params <- pirouette::create_alignment_params(
    root_sequence = pirouette::create_blocked_dna(length = 32),
    mutation_rate = 0.12
  )
  gen_model_select_params <- list(
    pirouette::create_gen_model_select_param(
      alignment_params = alignment_params
    )
  )
  pirouette:::check_model_select_params(gen_model_select_params)

  best_model_select_params <- list(pirouette::create_best_model_select_param())
  pirouette:::check_model_select_params(best_model_select_params)
  model_select_params <- list(gen_model_select_params, best_model_select_params)

  inference_params <- pirouette::create_inference_params(
    mrca_prior = beautier::create_mrca_prior(
      alignment_id = "to be added by pir_run",
      taxa_names = c("to", "be", "added", "by", "pir_run"),
      is_monophyletic = TRUE,
      mrca_distr = beautier::create_normal_distr(
        mean = crown_age,
        sigma = crown_age_sigma
      )
    ),
    mcmc = beautier::create_mcmc(chain_length = 12300)
  )

  error_measure_params <- create_error_measure_params(burn_in_fraction = 0.12)

  razzo_params <- create_razzo_params(
    mbd_params = mbd_params,
    twinning_params = twinning_params,
    alignment_params = alignment_params,
    model_select_params = model_select_params,
    inference_params = inference_params,
    error_measure_params = error_measure_params
  )

  expect_equal(mbd_params, razzo_params$mbd_params)
  expect_equal(twinning_params, razzo_params$twinning_params)
  expect_equal(alignment_params, razzo_params$alignment_params)
  expect_equal(model_select_params, razzo_params$model_select_params)
  expect_equal(inference_params, razzo_params$inference_params)
  expect_equal(error_measure_params, razzo_params$error_measure_params)
})
