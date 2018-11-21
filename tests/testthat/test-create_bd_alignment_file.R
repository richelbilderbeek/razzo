context("create_bd_alignment_file")

test_that("use", {

  parameters_filename <- create_tempfile("parameters.csv")
  bd_tree_filename <- create_tempfile("bd.tree")
  mbd_tree_filename <- create_tempfile("mbd.tree")
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(bd_tree_filename))

  bd_alignment_filename <- create_bd_alignment_file(
    parameters_filename = parameters_filename
  )
  expect_true(file.exists(bd_alignment_filename))
  expect_true(length(grep(
    pattern = "bd\\.fasta$",
    bd_alignment_filename, perl = TRUE, value = TRUE))
    > 0
  )
})
