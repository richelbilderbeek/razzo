context("raz_create_mbd_tree_files")

test_that("use", {
  mbd_filenames <- raz_create_mbd_tree_files(
    parameters_filename = raz_create_tempfile("parameters.csv")
  )
  mbd_tree_filename <- mbd_filenames$mbd_tree_filename
  mbd_l_matrix_filename <- mbd_filenames$mbd_l_matrix_filename
  expect_true(file.exists(mbd_tree_filename))
  expect_true(length(grep(
    pattern = "mbd\\.tree$",
    mbd_tree_filename, perl = TRUE, value = TRUE))
    > 0
  )
  expect_true(file.exists(mbd_l_matrix_filename))
  expect_true(length(grep(
    pattern = "mbd_l_matrix\\.csv$",
    mbd_l_matrix_filename, perl = TRUE, value = TRUE))
    > 0
  )
})
