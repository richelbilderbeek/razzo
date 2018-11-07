context("raz_combine_brts_and_topology")

test_that("check usage with brts coming from the same tree", {

  parameters <- razzo::raz_open_parameters_file(
    razzo::raz_get_path("parameters.csv")
  )
  parameters$seed <- 1
  tree <- razzo::raz_create_mbd_tree(parameters)$mbd_tree
  brts <- ape::branching.times(tree)

<<<<<<< HEAD
  max_seed <- 5
  for (seed in 1:max_seed) {
    parameters$seed <- seed
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
  }
})

test_that("all the tree features (but the branching times) are preserved", {

set.seed(1)
  age <- 10
  sim <- mbd::mbd_sim(
    pars = c(0.3, 0.1, 2, 0.1),
    n_0 = 2,
    age = age,
    cond = 1
  )
  tree <- sim$reconstructed_tree
  brts <- sort(c(
    age,
    runif(
      n = (length(sim$brts) - 1),
      min = 0,
      max = age - 0.001)
  ),
  decreasing = TRUE)
  new_tree <- raz_combine_brts_and_topology(
      brts = brts,
      tree = tree
  )
  testthat::expect_equal(
    new_tree$edge, tree$edge
  )
  testthat::expect_equal(
    new_tree$Nnode, tree$Nnode
  )
  testthat::expect_equal(
    new_tree$root.edge, tree$root.edge
  )
  testthat::expect_equal(
    new_tree$tip.label, tree$tip.label
=======
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
>>>>>>> f128bc821dff6891a72b6cbd9d9e2b09876434c2
  )
})
