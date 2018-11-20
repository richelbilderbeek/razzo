context("raz_create_mbd_tree_files")

test_that("use", {
  mbd_tree_filename <- raz_create_mbd_tree_file(
    parameters_filename = raz_create_tempfile("parameters.csv")
  )
  expect_true(file.exists(mbd_tree_filename))
  expect_true(length(grep(
    pattern = "mbd\\.tree$", mbd_tree_filename, perl = TRUE, value = TRUE))
    > 0
  )
})
