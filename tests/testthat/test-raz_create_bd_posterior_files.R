context("raz_create_bd_posterior_files")

test_that("use", {

  if (rappdirs::app_dir()$os == "win") {
  skip("Cannot do inference on Windows computers")
    # See https://github.com/richelbilderbeek/beastier/blob/master/doc/faq.md ,
    # section 'Why doesn't beastier support calling the Windows BEAST2.exe
    # file?'. Spoiler: because BEAST2.exe does not allow scripted use
  }

  parameters_filename <- raz_create_tempfile("parameters.csv")
  bd_alignment_filename <- raz_create_tempfile("bd.fasta")
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(bd_alignment_filename))

  bd_posterior_filenames <- raz_create_bd_posterior_files(
    parameters_filename = parameters_filename
  )
  expect_true(all(file.exists(bd_posterior_filenames)))
  expect_true(length(grep(
    pattern = "bd\\.trees$",
    bd_posterior_filenames, perl = TRUE, value = TRUE))
    > 0
  )
  expect_true(length(grep(
    pattern = "bd\\.log$",
    bd_posterior_filenames, perl = TRUE, value = TRUE))
    > 0
  )
  expect_true(length(grep(
    pattern = "bd_mar_log_lik\\.csv$",
    bd_posterior_filenames, perl = TRUE, value = TRUE))
    > 0
  )
})
