context("raz_create_input_files")

test_that("use", {
  parameters_filename <- "1.csv"
  input_filenames <- raz_create_input_files(parameters_filename)
  expect_equal(4, length(input_filenames))
  expect_true("1a.tree" %in% input_filenames)
  expect_true("1a.fasta" %in% input_filenames)
  expect_true("1b.tree" %in% input_filenames)
  expect_true("1b.fasta" %in% input_filenames)
})
