context("create_bd_alignment")

test_that("use", {

  alignment <- create_bd_alignment(
    parameters = open_parameters_file(get_path("parameters.csv")),
    mbd_tree = ape::read.tree(file = get_path("mbd.tree")), # NEW
    bd_tree = ape::read.tree(file = get_path("bd.tree"))
  )
  expect_equal(class(alignment), "DNAbin")
})
