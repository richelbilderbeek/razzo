context("create_mbd_alignment")

test_that("use", {
  skip("Not anymore")
  alignment <- create_mbd_alignment(
    parameters = open_parameters_file(get_path("parameters.csv")),
    mbd_tree = ape::read.tree(file = get_path("mbd.tree")),
    bd_tree = ape::read.tree(file = get_path("bd.tree")) # NEW
  )
  expect_equal(class(alignment), "DNAbin")
})
