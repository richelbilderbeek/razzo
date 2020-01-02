test_that("matches article, generative", {
  folder_name <- get_pff_tempfile()
  nested_sampling_mcmc <- create_razzo_nested_sampling_mcmc(
    folder_name = folder_name,
    model_type = "generative"
  )
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

test_that("file names, generative", {
  folder_name <- get_pff_tempfile()
  nested_sampling_mcmc <- create_razzo_nested_sampling_mcmc(
    folder_name = folder_name,
    model_type = "generative"
  )
  expect_equal(
    file.path(folder_name, "mbd_gen_evidence.log"),
    nested_sampling_mcmc$tracelog$filename
  )
  expect_equal(
    file.path(folder_name, "mbd_gen_evidence.trees"),
    nested_sampling_mcmc$treelog$filename
  )
})

test_that("file names, candidate", {
  folder_name <- get_pff_tempfile()
  nested_sampling_mcmc <- create_razzo_nested_sampling_mcmc(
    folder_name = folder_name,
    model_type = "candidate",
    index = 1
  )
  expect_equal(
    file.path(folder_name, "mbd_best_1_evidence.log"),
    nested_sampling_mcmc$tracelog$filename
  )
  expect_equal(
    file.path(folder_name, "mbd_best_1_evidence.trees"),
    nested_sampling_mcmc$treelog$filename
  )
})

test_that("file names, candidate, 2nd", {
  folder_name <- get_pff_tempfile()
  nested_sampling_mcmc <- create_razzo_nested_sampling_mcmc(
    folder_name = folder_name,
    model_type = "candidate",
    index = 2
  )
  expect_equal(
    file.path(folder_name, "mbd_best_2_evidence.log"),
    nested_sampling_mcmc$tracelog$filename
  )
  expect_equal(
    file.path(folder_name, "mbd_best_2_evidence.trees"),
    nested_sampling_mcmc$treelog$filename
  )
})
