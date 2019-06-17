context("test-check_razzo_params")

test_that("use", {
  expect_silent(
    check_razzo_params(razzo_params = create_test_razzo_params())
  )

  mbd_params <- create_test_mbd_params()
  pir_params <- pirouette::create_test_pir_params(
    experiments = list(
      pirouette::create_test_gen_experiment(
        inference_model = beautier::create_inference_model(
          mrca_prior = beautier::create_mrca_prior(
            mrca_distr = beautier::create_normal_distr(mean = 15.0, sigma = 0.001),
            is_monophyletic = TRUE
          ),
          mcmc = beautier::create_mcmc(
            chain_length = 2000, store_every = 1000
          )
        )
      )
    ),
    twinning_params = pirouette::create_twinning_params()
  )
  misc_params <- create_misc_params()

  expect_silent(
    create_razzo_params(
      mbd_params = mbd_params,
      pir_params = pir_params,
      misc_params = misc_params
    )
  )

  # Check elements
  expect_error(
    check_razzo_params(razzo_params = list()),
    "'mbd_params' must be an element of a 'razzo_params'"
  )

  expect_error(
    check_razzo_params(razzo_params = list(mbd_params = mbd_params)),
    "'pir_params' must be an element of a 'razzo_params'"
  )

  expect_error(
    check_razzo_params(razzo_params = list(
      mbd_params = mbd_params, pir_params = pir_params)
    ),
    "'misc_params' must be an element of a 'razzo_params'"
  )

})
