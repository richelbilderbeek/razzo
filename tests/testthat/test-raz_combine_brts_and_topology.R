context("raz_combine_brts_and_topology")

test_that("check usage with brts coming from the same tree", {

  parameters <- razzo::raz_open_parameters_file(
    razzo::raz_get_path("parameters.csv")
  )
  parameters$seed <- 1
  tree <- razzo::raz_create_mbd_tree(parameters)$mbd_tree
  brts <- ape::branching.times(tree)

  test <- raz_combine_brts_and_topology(
    brts = brts,
    tree = tree
  )

  testthat::expect_true(
    all(test$edge == tree$edge)
  )
  testthat::expect_true(
    all(test$Nnode == tree$Nnode)
  )
  testthat::expect_true(
    all(test$tip.label == tree$tip.label)
  )
  testthat::expect_true(
    all(test$root.edge == tree$root.edge)
  )
  testthat::expect_true(
    max(unname(test$edge.length) - tree$edge.length) <
      max(tree$edge.length * 1e-6)
  )
})
