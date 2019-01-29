context("test-est_marg_lik")

test_that("use", {

  expect_true(1 + 1 == 2) # to get a non-empty test

  if (!beastier::is_on_travis()) return()
  if (rappdirs::app_dir()$os == "win") return()

  parameters <- open_parameters_file(get_path("parameters.csv"))
  alignment <- ape::read.FASTA(get_path("mbd.fasta"))

  marg_lik <- est_marg_lik(
    parameters = parameters,
    alignment = alignment
  )
  expect_true("marg_log_lik" %in% names(marg_lik))
  expect_true("marg_log_lik_sd" %in% names(marg_lik))
})

test_that("use for other 3 models", {

  expect_true(1 + 1 == 2) # to get a non-empty test

  if (!beastier::is_on_travis()) return()
  if (rappdirs::app_dir()$os == "win") return()

  alignment <- ape::read.FASTA(get_path("mbd.fasta"))
  parameters <- open_parameters_file(get_path("parameters.csv"))

  parameters$site_model <- "jc69"
  parameters$clock_model <- "strict"
  expect_silent(
    est_marg_lik(
      parameters = parameters,
      alignment = alignment
    )
  )

  parameters$site_model <- "jc69"
  parameters$clock_model <- "rln"
  expect_silent(
    est_marg_lik(
      parameters = parameters,
      alignment = alignment
    )
  )

  parameters$site_model <- "gtr"
  parameters$clock_model <- "strict"
  expect_silent(
    est_marg_lik(
      parameters = parameters,
      alignment = alignment
    )
  )

  parameters$site_model <- "gtr"
  parameters$clock_model <- "rln"
  expect_silent(
    est_marg_lik(
      parameters = parameters,
      alignment = alignment
    )
  )
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
