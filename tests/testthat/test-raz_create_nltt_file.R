context("raz_create_nltt_file")

test_that("use", {
  trees_filename <- "1a.trees"
  nltt_filename <- raz_create_nltt_file(trees_filename)
  expect_equal("1a_nltts.csv", nltt_filename)
})
