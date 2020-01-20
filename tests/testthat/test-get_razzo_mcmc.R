test_that("use", {
  mcmc <- get_razzo_mcmc(model_type = "generative")
  expect_equal(mcmc$tracelog$log_every, get_razzo_mcmc_store_every())
  expect_equal(mcmc$treelog$log_every, get_razzo_mcmc_store_every())
})
