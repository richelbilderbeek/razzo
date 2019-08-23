test_that("use", {
  nested_sampling_mcmc <- create_razzo_nested_sampling_mcmc()
  expect_true(beautier::is_nested_sampling_mcmc(nested_sampling_mcmc))
  expect_equal(nested_sampling_mcmc$store_every, 1000)
  expect_equal(nested_sampling_mcmc$epsilon, 1e-12)
  expect_equal(nested_sampling_mcmc$particle_count, 1)
  expect_equal(nested_sampling_mcmc$sub_chain_length, 5000)
})
