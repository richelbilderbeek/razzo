context("raz_create_bd_tree_file")

test_that("use", {

  parameters_filename <- raz_create_tempfile("parameters.csv")
  raz_create_tempfile("mbd.tree")
  raz_create_tempfile("mbd_l_matrix.csv")

  bd_tree_filename <- raz_create_bd_tree_file(
    parameters_filename = raz_create_tempfile("parameters.csv")
  )
  expect_true(file.exists(bd_tree_filename))
  expect_true(length(grep(
    pattern = "bd\\.tree$", bd_tree_filename, perl = TRUE, value = TRUE))
    > 0
  )

})
