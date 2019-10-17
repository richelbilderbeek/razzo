test_that("matches article", {
  nested_sampling_mcmc <- create_razzo_nested_sampling_mcmc()
  expect_true(beautier::is_nested_sampling_mcmc(nested_sampling_mcmc))
  expect_equal(
    nested_sampling_mcmc$chain_length,
    get_razzo_mcmc_chain_length()
  )
  expect_equal(
    nested_sampling_mcmc$store_every,
    get_razzo_mcmc_store_every()
  )
  expect_equal(nested_sampling_mcmc$epsilon, 1e-12)
  expect_equal(nested_sampling_mcmc$particle_count, 1)
  expect_equal(nested_sampling_mcmc$sub_chain_length, 5000)
})
