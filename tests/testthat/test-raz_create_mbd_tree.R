context("raz_create_mbd_tree")

test_that("creates a tree", {

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  parameters$seed <- 1
  mbd_sim <- raz_create_mbd_tree(parameters)
  expect_true("mbd_tree" %in% names(mbd_sim))
  expect_equal(
    max(ape::branching.times(mbd_sim$mbd_tree)),
    parameters$crown_age
  )
  expect_true("mbd_l_matrix" %in% names(mbd_sim))
  expect_equal(class(mbd_sim$mbd_tree), "phylo")

  nrow(mbd_sim$mbd_l_matrix)
  # Should preferably be a data.frame or tibble
  expect_equal(class(mbd_sim$mbd_l_matrix), "matrix")
})
