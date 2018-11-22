context("test-create_bd_marg_lik_file")

test_that("must create file", {

  if (!ribir::is_on_travis()) return()
  if (rappdirs::app_dir()$os == "win") return()

  parameters_filename <- create_tempfile("parameters.csv")
  bd_alignment_filename <- create_tempfile("bd.fasta")
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(bd_alignment_filename))

  bd_marg_lik_filename <- create_bd_marg_lik_file(# nolint internal function
    parameters_filename = parameters_filename
  )
  expect_true(file.exists(bd_marg_lik_filename))
  expect_true(length(grep(
    pattern = "bd_marg_lik\\.csv$",
    bd_marg_lik_filename, perl = TRUE, value = TRUE))
    > 0
  )
})
