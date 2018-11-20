context("raz_utils")

test_that("raz_get_site_models", {
  expect_true(
    length(raz_get_site_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(raz_get_site_models()) # nolint internal function
  )
})

test_that("raz_get_clock_models", {
  expect_true(
    length(raz_get_clock_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(raz_get_clock_models()) # nolint internal function
  )
})

test_that("raz_get_gen_models", {
  expect_true(
    length(raz_get_gen_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(raz_get_gen_models()) # nolint internal function
  )
})

test_that("bd_phylo2L", {
  max_seed <- 10
  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  for (seed in 1:max_seed) {

    # create data
    parameters$seed <- seed
    mbd_sim <- raz_create_mbd_tree(
      parameters = parameters
    )
    mbd_tree <- mbd_sim$mbd_tree
    mbd_l_matrix <- mbd_sim$mbd_l_matrix
    bd_sim <- raz_create_bd_tree(
      parameters = parameters,
      mbd_tree = mbd_tree,
      mbd_l_matrix = mbd_l_matrix
    )
    bd_tree <- bd_sim$bd_tree
    bd_l_matrix <- bd_sim$bd_l_matrix

    # test phylo -> L -> phylo
    bd_test_tree <- DDD::L2phylo(bd_phylo2L(bd_tree), dropextinct = FALSE)
    expect_equal(
      bd_test_tree$edge,
      bd_tree$edge
    )
    expect_equal(
      bd_test_tree$edge.length,
      unname(bd_tree$edge.length)
    )
    expect_equal(
      bd_test_tree$Nnode,
      bd_tree$Nnode
    )
    expect_equal(
      bd_test_tree$root.edge,
      bd_tree$root.edge
    )

    # test L -> phylo -> L
    test_bd_l_matrix <- bd_phylo2L(DDD::L2phylo(
      bd_l_matrix,
      dropextinct = FALSE))
    expect_equal(
      test_bd_l_matrix,
      bd_l_matrix
    )
  }
})
