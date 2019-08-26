test_that("use", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  razzo_paramses <- create_test_razzo_paramses()
  expect_equal(length(razzo_paramses), 2)
})

test_that("values", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  razzo_paramses <- create_test_razzo_paramses()
  expect_equal(length(razzo_paramses[[1]]$pir_params$experiments), 3)

  experiment_1_1 <- razzo_paramses[[1]]$pir_params$experiments[[1]]
  experiment_1_2 <- razzo_paramses[[1]]$pir_params$experiments[[2]]
  experiment_1_3 <- razzo_paramses[[1]]$pir_params$experiments[[3]]
  expect_true(experiment_1_1$inference_model$mcmc$chain_length < 10000)
  expect_true(experiment_1_2$inference_model$mcmc$chain_length < 10000)
  expect_true(experiment_1_3$inference_model$mcmc$chain_length < 10000)

  experiment_2_1 <- razzo_paramses[[1]]$pir_params$experiments[[1]]
  experiment_2_2 <- razzo_paramses[[1]]$pir_params$experiments[[2]]
  experiment_2_3 <- razzo_paramses[[1]]$pir_params$experiments[[3]]
  expect_true(experiment_2_1$inference_model$mcmc$chain_length < 10000)
  expect_true(experiment_2_2$inference_model$mcmc$chain_length < 10000)
  expect_true(experiment_2_3$inference_model$mcmc$chain_length < 10000)
})
