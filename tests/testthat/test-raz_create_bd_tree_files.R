context("raz_create_bd_tree_files")

test_that("use", {

  parameters_filename <- raz_create_tempfile("parameters.csv")
  mbd_tree_filename <- raz_create_tempfile("mbd.tree")
  mbd_l_matrix_filename <- raz_create_tempfile("mbd_l_matrix.csv")
  expect_true(file.exists(parameters_filename))
  expect_true(file.exists(mbd_tree_filename))
  expect_true(file.exists(mbd_l_matrix_filename))

  bd_tree_filename <- raz_create_bd_tree_file(
    parameters_filename = parameters_filename
  )
  expect_true(file.exists(bd_tree_filename))
  expect_true(length(grep(
    pattern = "bd\\.tree$", bd_tree_filename, perl = TRUE, value = TRUE))
    > 0
  )

})
