context("raz_create_mbd_tree_events")

test_that("creates a tree", {

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  parameters$seed <- 1
  nu_events <- 5
  mbd_sim <- razzo:::raz_create_mbd_tree_events(
    parameters = parameters,
    nu_events = nu_events
  )
  expect_true("mbd_tree" %in% names(mbd_sim))
  expect_equal(
    max(ape::branching.times(mbd_sim$mbd_tree)),
    parameters$crown_age
  )
  expect_true("mbd_l_matrix" %in% names(mbd_sim))
  expect_equal(class(mbd_sim$mbd_tree), "phylo")

  # Should preferably be a data.frame or tibble
  expect_equal(class(mbd_sim$mbd_l_matrix), "matrix")

})

test_that("tree must have a predefined number of nu events", {

  parameters <- razzo::raz_open_parameters_file(raz_get_path("parameters.csv"))
  # Calculate the number of expected triggered speciation events
  exp_n_spec_events <- round(parameters$crown_age * parameters$nu) / 5

  mbd_sim <- razzo:::raz_create_mbd_tree_events(
    parameters = parameters,
    nu_events = exp_n_spec_events
  )
  mbd_tree <- mbd_sim$mbd_tree
  mbd_brts <- DDD:::L2brts(
    unname(mbd_sim$mbd_l_matrix),
    dropextinct = TRUE
  )

  # Count the number of actual triggered speciation events
  n_spec_events <- mbd::mbd_count_n_spec_events(mbd_brts)

  expect_equal(exp_n_spec_events, n_spec_events)
})
