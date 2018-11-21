context("test-raz_est_marg_lik")

test_that("use", {

  # Too long to run locally
  if (!ribir::is_on_travis()) return()

  # Cannot use Nested Sampling on Windows
  if (rappdirs::app_dir()$os == "win") return()

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  alignment <- ape::read.FASTA(raz_get_path("mbd.fasta"))

  marg_lik <- raz_est_marg_lik(
    parameters = parameters,
    alignment = alignment
  )
  expect_true("marg_log_lik" %in% names(marg_lik))
  expect_true("marg_log_lik_sd" %in% names(marg_lik))
})

test_that("abuse", {

  # Cannot use Nested Sampling on Windows
  if (rappdirs::app_dir()$os == "win") return()

  parameters <- raz_open_parameters_file(raz_get_path("parameters.csv"))
  alignment <- ape::read.FASTA(raz_get_path("mbd.fasta"))
  parameters$clock_model <- "nonsense"

  expect_error(
    raz_est_marg_lik(
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
  parameters$site_model <- "nonsense"

  expect_error(
    raz_est_marg_lik(
      parameters = parameters,
      alignment = alignment
    ),
    paste0(
      "'site_model' must be among the following: ",
      paste(raz_get_site_models(), collapse = ", ")
    )
  )
})
