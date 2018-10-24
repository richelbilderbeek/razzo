context("raz_create_mbd_nltts_file")

test_that("use", {

  # Create input files
  parameters_filename <- raz_create_tempfile("parameters.csv")
  mbd_tree_filename <- raz_create_tempfile("mbd.tree")
  mbd_trees_filename <- raz_create_tempfile("mbd.trees")
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(mbd_tree_filename))
  testit::assert(file.exists(mbd_trees_filename))

  # Run
  mbd_nltts_filename <- raz_create_mbd_nltts_file(
    parameters_filename = parameters_filename
  )

  # Check
  expect_true(all(file.exists(mbd_nltts_filename)))
  expect_true(length(grep(
    pattern = "mbd_nltts\\.csv$",
    mbd_nltts_filename, perl = TRUE, value = TRUE))
    > 0
  )
})
