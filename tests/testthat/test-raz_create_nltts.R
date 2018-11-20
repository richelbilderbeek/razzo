context("raz_create_nltts")

test_that("use", {

  tree <- ape::read.tree(file = raz_get_path("mbd.tree"))
  posterior_trees <- tracerer::parse_beast_trees(
    filename = raz_get_path("mbd.trees")
  )
  nltts <- raz_create_nltts(
    tree = tree,
    posterior_trees = posterior_trees
  )
  expect_equal("numeric", class(nltts))
  expect_true(all(nltts >= 0.0))
  expect_true(all(nltts <= 1.0))
})
