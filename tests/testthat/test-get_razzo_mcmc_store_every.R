test_that("matches article", {
  expect_equal(
    get_razzo_mcmc_store_every(),
    get_razzo_mcmc_chain_length() / 1000
  )
})
