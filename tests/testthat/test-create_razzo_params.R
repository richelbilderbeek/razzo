context("test-create_razzo_params")

test_that("use", {

  crown_age <- 15
  crown_age_sigma <- 0.01
  mbd_params <- create_mbd_params(
    lambda = 0.1,
    mu = 0.15,
    nu = 0.2,
    q = 0.01,
    crown_age = crown_age,
    cond = 1
  )
  experiment <- pirouette::create_experiment(
    model_type = "generative",
    run_if = "always",
    do_measure_evidence = FALSE,
    inference_model = beautier::create_inference_model(
      mrca_prior = beautier::create_mrca_prior(
        mrca_distr = beautier::create_normal_distr(
          mean = crown_age,
          sigma = crown_age_sigma
        ),
        is_monophyletic = TRUE
      )
    )
  )

  experiments <- list(experiment)
  pir_params <- pirouette::create_pir_params(
    alignment_params = pirouette::create_test_alignment_params(),
    experiments = experiments

  )
  misc_params <- list()
  misc_params$tree_filename <- "mbd.tree"

  razzo_params <- create_razzo_params(
    mbd_params = mbd_params,
    pir_params = pir_params,
    misc_params = misc_params
  )

  expect_equal(mbd_params, razzo_params$mbd_params)
  expect_equal(pir_params, razzo_params$pir_params)
  expect_equal(misc_params, razzo_params$misc_params)
})
