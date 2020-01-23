test_that("matches #346", {
  skip("Changed for https://github.com/richelbilderbeek/razzo/issues/346")

  expect_equal(
    1e7,
    get_razzo_mcmc_chain_length()
  )
})
