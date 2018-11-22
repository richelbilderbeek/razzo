context("test-est_marg_lik")

test_that("use", {

  if (!ribir::is_on_travis()) return()
  if (rappdirs::app_dir()$os == "win") return()

  skip("Test fails. ISSUE: #119")
  parameters <- open_parameters_file(get_path("parameters.csv"))
  alignment <- ape::read.FASTA(get_path("mbd.fasta"))

  marg_lik <- est_marg_lik(
    parameters = parameters,
    alignment = alignment
  )
  expect_true("marg_log_lik" %in% names(marg_lik))
  expect_true("marg_log_lik_sd" %in% names(marg_lik))
})

test_that("abuse", {

  # Fixture
  alignment <- ape::read.FASTA(get_path("mbd.fasta"))

  # Parameters with nonsense clock model
  parameters <- open_parameters_file(get_path("parameters.csv"))
  parameters$clock_model <- "nonsense"
  expect_error(
    est_marg_lik(
      parameters = parameters,
      alignment = alignment
    ),
    paste0(
      "'clock_model' must be among the following: ",
      paste(get_clock_models(), collapse = ", ")
    )
  )

  # Parameters with nonsense site model
  parameters <- open_parameters_file(get_path("parameters.csv"))
  parameters$site_model <- "nonsense"

  expect_error(
    est_marg_lik(
      parameters = parameters,
      alignment = alignment
    ),
    paste0(
      "'site_model' must be among the following: ",
      paste(get_site_models(), collapse = ", ")
    )
  )
})
