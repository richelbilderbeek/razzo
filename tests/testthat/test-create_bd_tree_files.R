context("create_bd_tree_files")

test_that("use", {

  parameters_filename <- create_tempfile("parameters.csv")
  mbd_tree_filename <- create_tempfile("mbd.tree")
  mbd_l_matrix_filename <- create_tempfile("mbd_l_matrix.csv")
  expect_true(file.exists(parameters_filename))
  expect_true(file.exists(mbd_tree_filename))
  expect_true(file.exists(mbd_l_matrix_filename))

  bd_filenames <- create_bd_tree_files(
    parameters_filename = parameters_filename
  )
  bd_tree_filename <- bd_filenames$bd_tree_filename
  bd_l_matrix_filename <- bd_filenames$bd_l_matrix_filename
  expect_true(file.exists(bd_tree_filename))
  expect_true(length(grep(
    pattern = "bd\\.tree$",
    bd_tree_filename, perl = TRUE, value = TRUE))
    > 0
  )
  expect_true(file.exists(bd_l_matrix_filename))
  expect_true(length(grep(
    pattern = "bd_l_matrix\\.csv$",
    bd_l_matrix_filename, perl = TRUE, value = TRUE))
    > 0
  )
})
