context("create_bd_nltts_file")

test_that("use", {

  # Create input files
  parameters_filename <- create_tempfile("parameters.csv")
  bd_tree_filename <- create_tempfile("bd.tree")
  bd_trees_filename <- create_tempfile("bd.trees")
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(bd_tree_filename))
  testit::assert(file.exists(bd_trees_filename))

  # Run
  bd_nltts_filename <- create_bd_nltts_file(
    parameters_filename = parameters_filename
  )

  # Check
  expect_true(all(file.exists(bd_nltts_filename)))
  expect_true(length(grep(
    pattern = "bd_nltts\\.csv$",
    bd_nltts_filename, perl = TRUE, value = TRUE))
    > 0
  )
})
