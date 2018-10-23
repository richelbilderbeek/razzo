context("raz_create_bd_alignment")

test_that("use", {

  alignment <- raz_create_bd_alignment(
    parameters = raz_open_parameters_file(raz_get_path("parameters.csv")),
    bd_tree = ape::read.tree(file = raz_get_path("bd.tree"))
  )
  expect_equal(class(alignment), "DNAbin")
})
