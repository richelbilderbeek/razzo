context("test-raz_est_marg_lik")

test_that("use", {

  # Too long to run locally
  if (!ribir::is_on_travis()) return()

  # Cannot use Nested Sampling on Windows
  if (rappdirs::app_dir()$os == "win") return()

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  alignment <- ape::read.FASTA(raz_get_path("mbd.fasta"))
  parameters$chain_length <- 10000

  marg_lik <- raz_est_marg_lik(
    parameters = parameters,
    alignment = alignment
  )
  expect_true("log_lik" %in% names(marg_lik))
  expect_true("log_error" %in% names(marg_lik))

  skip("TODO: add 'raz_est_marg_lik'. Issue 36, #36")

})

test_that("abuse", {

  skip("TODO: add 'raz_est_marg_lik'. Issue 36, #36")
  if (!ribir::is_on_travis()) return()

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  alignment <- ape::read.FASTA(raz_get_path("mbd.fasta"))
  parameters$chain_length <- 2000
  parameters$clock_model <- "nonsense"

  expect_error(
    posterior <- raz_est_marg_lik(
      parameters = parameters,
      alignment = alignment
    ),
    paste0(
      "'clock_model' must be among the following: ",
      paste(raz_get_clock_models(), collapse = ", ")
    )
  )

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  alignment <- ape::read.FASTA(raz_get_path("mbd.fasta"))
  parameters$chain_length <- 2000
  parameters$site_model <- "nonsense"

  expect_error(
    posterior <- raz_est_marg_lik(
      parameters = parameters,
      alignment = alignment
    ),
    paste0(
      "'site_model' must be among the following: ",
      paste(raz_get_site_models(), collapse = ", ")
    )
  )
})
