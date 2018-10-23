context("raz_create_mbd_tree")

test_that("creates a tree", {

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  tree <- raz_create_mbd_tree(parameters)
  expect_equal(class(tree), "phylo")
}

test_that("must have as much multiple bursts as predicted by nu", {

  parameters <- razzo::raz_open_parameters_file(raz_get_path("parameters.csv"))

  tree <- razzo::raz_create_mbd_tree(parameters)

  # Calculate the number of expected triggered speciation events
  exp_n_spec_events <- parameters$crown_age / parameters$nu

  skip("TODO: Issue #: Create trees with expected number of speciation events")
  # Count the number of actual triggered speciation events
  n_spec_events <- mbd::mbd_count_n_spec_events(mbd_tree)

  expect_equal(exp_n_spec_events, n_spec_events)
})
