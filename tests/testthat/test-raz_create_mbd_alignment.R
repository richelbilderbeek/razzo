context("raz_create_mbd_alignment")

test_that("use", {

  skip("TODO: #62, Issue 62")

  alignment <- raz_create_mbd_alignment(
    parameters = raz_open_parameters_file(raz_get_path("parameters.csv")),
    mbd_tree = ape::read.tree(file = raz_get_path("mbd.tree")),
    bd_tree = ape::read.tree(file = raz_get_path("bd.tree")) # NEW
  )
  expect_equal(class(alignment), "DNAbin")
})
