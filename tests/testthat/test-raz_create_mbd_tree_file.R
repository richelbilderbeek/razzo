context("raz_create_mbd_tree_file")

test_that("use", {
  mbd_tree_filename <- razzo::raz_create_mbd_tree_file(
    parameters_filename = raz_create_tempfile("parameters.csv")
  )
  expect_true(file.exists(mbd_tree_filename))
})
