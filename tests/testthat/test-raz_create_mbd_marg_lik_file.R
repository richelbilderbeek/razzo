context("test-raz_create_mbd_marg_lik_file")

test_that("must create file", {

  parameters_filename <- raz_create_tempfile("parameters.csv")
  mbd_alignment_filename <- raz_create_tempfile("mbd.fasta")
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(mbd_alignment_filename))

  mbd_marg_lik_filename <- raz_create_mbd_marg_lik_file( # nolint internal function
    parameters_filename = parameters_filename
  )
  expect_true(file.exists(mbd_marg_lik_filename))
  expect_true(length(grep(
    pattern = "mbd_marg_lik\\.csv$",
    mbd_marg_lik_filename, perl = TRUE, value = TRUE))
    > 0
  )
})
