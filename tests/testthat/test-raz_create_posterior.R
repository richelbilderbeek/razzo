context("raz_create_posterior")

test_that("use", {

  if (!ribir::is_on_travis()) return()

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  alignment <- ape::read.FASTA(raz_get_path("mbd.fasta"))

  parameters$chain_length <- 2000
  posterior <- raz_create_posterior(
    parameters = parameters,
    alignment = alignment
  )
  expect_true("trees" %in% names(posterior))
  expect_true("estimates" %in% names(posterior))
})
