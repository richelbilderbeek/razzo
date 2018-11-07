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

  if (1 == 2) {
    # To re-create the files in inst/extdata
    ape::plot.phylo(mbd_sim$mbd_tree)
    if (rappdirs::app_dir()$os == "win") {
      ape::write.tree(phy = mbd_sim$mbd_tree,
                      file = raz_get_path("mbd.tree"))
      write.csv(x = mbd_sim$mbd_l_matrix,
                file = raz_get_path("mbd_l_matrix.csv"))
    } else {
      ape::write.tree(phy = mbd_sim$mbd_tree, file = "~/mbd.tree")
      write.csv(x = mbd_sim$mbd_l_matrix, file = "~/mbd_l_matrix.csv")
    }
  }
})

test_that("must have as much multiple bursts as predicted by nu", {

  parameters <- razzo::raz_open_parameters_file(raz_get_path("parameters.csv"))

  mbd_sim <- razzo::raz_create_mbd_tree(parameters)
  tree <- mbd_sim$mbd_tree

  # Calculate the number of expected triggered speciation events
  exp_n_spec_events <- parameters$crown_age / parameters$nu

  skip(
    "TODO: Issue #24: Create trees with expected number of speciation events"
  )
  # Count the number of actual triggered speciation events
  n_spec_events <- mbd::mbd_count_n_spec_events(mbd_tree)

  expect_equal(exp_n_spec_events, n_spec_events)
})
