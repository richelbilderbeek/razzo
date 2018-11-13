context("raz_create_mbd_posterior_files")

test_that("use", {

  if (rappdirs::app_dir()$os == "win") {
    skip("Cannot do inference on Windows computers")
    # See https://github.com/richelbilderbeek/beastier/blob/master/doc/faq.md ,
    # section 'Why doesn't beastier support calling the Windows BEAST2.exe
    # file?'. Spoiler: because BEAST2.exe does not allow scripted use
  }

  if (!ribir::is_on_travis()) return()

  # Create input files
  parameters_filename <- raz_create_tempfile("parameters.csv") # nolint internal function
  mbd_alignment_filename <- raz_create_tempfile("mbd.fasta") # nolint internal function
  testit::assert(file.exists(parameters_filename))
  testit::assert(file.exists(mbd_alignment_filename))

  # Run
  mbd_posterior_filenames <- raz_create_mbd_posterior_files( # nolint internal function
    parameters_filename = parameters_filename
  )

  # Check
  expect_true(all(file.exists(mbd_posterior_filenames)))
  expect_true(length(grep(
    pattern = "mbd\\.trees$",
    mbd_posterior_filenames, perl = TRUE, value = TRUE))
    > 0
  )
  expect_true(length(grep(
    pattern = "mbd\\.log$",
    mbd_posterior_filenames, perl = TRUE, value = TRUE))
    > 0
  )
  expect_true(length(grep(
    pattern = "mbd_mar_log_lik\\.csv$",
    mbd_posterior_filenames, perl = TRUE, value = TRUE))
    > 0
  )
})
