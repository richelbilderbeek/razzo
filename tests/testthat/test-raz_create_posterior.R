context("raz_create_posterior")

test_that("use", {

  if (rappdirs::app_dir()$os == "win") {
    skip("Cannot do inference on Windows computers")
    # See https://github.com/richelbilderbeek/beastier/blob/master/doc/faq.md ,
    # section 'Why doesn't beastier support calling the Windows BEAST2.exe
    # file?'. Spoiler: because BEAST2.exe does not allow scripted use
    # and beast.jar does not allow for use of BEAST2 packages
  }

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  alignment <- ape::read.FASTA(raz_get_path("mbd.fasta"))

  parameters$chain_length <- 2000
  posterior <- raz_create_posterior(
    parameters = parameters,
    alignment = alignment
  )
  expect_true("trees" %in% names(posterior))
  expect_true("estimates" %in% names(posterior))
  expect_true("ns" %in% names(posterior))

})
