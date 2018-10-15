context("raz_create_inference_files")

test_that("use", {
  fasta_filename <- "1a.fasta"
  inference_filenames <- raz_create_inference_files(fasta_filename)
  expect_true("1a.trees" %in% inference_filenames)
  expect_true("1a.log" %in% inference_filenames)
  expect_true("1a_mar_lik.csv" %in% inference_filenames)
})
