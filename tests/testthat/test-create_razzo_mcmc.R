test_that("use, generative", {
  folder_name <- peregrine::get_pff_tempfile()
  mcmc <- create_razzo_mcmc(
    model_type = "generative",
    folder_name = folder_name
  )
  expect_equal(
    mcmc$tracelog$filename,
    file.path(folder_name, "mbd_gen.log")
  )
  expect_equal(
    mcmc$treeslog$filename,
    file.path(folder_name, "mbd_gen.trees")
  )
})

test_that("use, candidate", {
  folder_name <- peregrine::get_pff_tempfile()
  mcmc <- create_razzo_mcmc(
    model_type = "candidate",
    folder_name = folder_name
  )
  expect_equal(
    mcmc$tracelog$filename,
    file.path(folder_name, "mbd_best.log")
  )
  expect_equal(
    mcmc$treeslog$filename,
    file.path(folder_name, "mbd_best.trees")
  )
})
