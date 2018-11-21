context("create_mbd_alignment_file")

test_that("must create file", {

  parameters_filename <- create_tempfile("parameters.csv")
  mbd_tree_filename <- create_tempfile("mbd.tree")
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(mbd_tree_filename))

  mbd_alignment_filename <- create_mbd_alignment_file( # nolint internal function
    parameters_filename = parameters_filename
  )
  expect_true(file.exists(mbd_alignment_filename))
  expect_true(length(grep(
    pattern = "mbd\\.fasta$",
    mbd_alignment_filename, perl = TRUE, value = TRUE))
    > 0
  )
})
