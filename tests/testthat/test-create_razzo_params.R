context("test-create_razzo_params")

test_that("use", {

<<<<<<< HEAD
  mbd_params <- becosys::create_mbd_params(
=======
  skip("Issue #127")
  crown_age <- 15
  crown_age_sigma <- 0.01
  mbd_params <- create_mbd_params(
>>>>>>> develop
    lambda = 0.1,
    mu = 0.15,
    nu = 0.2,
    q = 0.01,
    crown_age = crown_age,
    cond = 1
  )
  pir_params <- pirouette::create_pir_params(
    alignment_params = create_test_alignment_params()
  )
<<<<<<< HEAD
  razzo_params <- create_razzo_params(
    mbd_params = mbd_params,
    pir_params = pir_params
=======
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
  misc_params <- list()
  misc_params$tree_filename <- "mbd.tree"

  razzo_params <- create_razzo_params(
    mbd_params = mbd_params,
    twinning_params = twinning_params,
    alignment_params = alignment_params,
    model_select_params = model_select_params,
    inference_params = inference_params,
    error_measure_params = error_measure_params,
    misc_params = misc_params
>>>>>>> develop
  )

  expect_equal(mbd_params, razzo_params$mbd_params)
  expect_equal(pir_params, razzo_params$pir_params)
})
