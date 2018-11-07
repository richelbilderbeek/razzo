context("raz_create_bd_tree")

test_that("use", {

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  parameters$seed <- 12
  mbd_tree <- ape::read.tree(file = raz_get_path("mbd.tree"))
  mbd_l_matrix <- as.matrix(
    utils::read.csv(file = raz_get_path("mbd_l_matrix.csv")))
  # Remove the first column?
  mbd_l_matrix <- mbd_l_matrix[, -1]

  bd_tree <- raz_create_bd_tree(
    parameters = parameters,
    mbd_tree = mbd_tree,
    mbd_l_matrix = mbd_l_matrix
  )
  expect_equal(class(bd_tree), "phylo")
  expect_equal(
    max(bd_tree$edge.length),
    max(mbd_tree$edge.length)
  )
})
