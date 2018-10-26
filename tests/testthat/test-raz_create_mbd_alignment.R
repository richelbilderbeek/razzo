context("raz_create_mbd_alignment")

test_that("use", {

  alignment <- raz_create_mbd_alignment(
    parameters = raz_open_parameters_file(raz_get_path("parameters.csv")),
    mbd_tree = ape::read.tree(file = raz_get_path("mbd.tree"))
  )
  expect_equal(class(alignment), "DNAbin")
})
