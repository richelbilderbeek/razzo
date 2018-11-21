context("test-est_marg_lik")

test_that("use", {

  if (!ribir::is_on_travis()) {
    skip("It runs only on travis")
  } else {
    skip("Test fails. ISSUE: #119")
    parameters <- open_parameters_file(get_path("parameters.csv"))
    alignment <- ape::read.FASTA(get_path("mbd.fasta"))
    parameters$chain_length <- 10000

    marg_lik <- est_marg_lik(
      parameters = parameters,
      alignment = alignment
    )
    expect_true("log_lik" %in% names(marg_lik))
    expect_true("log_error" %in% names(posterior))
    expect_true("ess" %in% names(posterior))
  }
})

test_that("abuse", {

  if (!ribir::is_on_travis()) {
    skip("It runs only on travis")
  } else {
    skip("Test fails. ISSUE: #119")
    parameters <- open_parameters_file(get_path("parameters.csv"))
    alignment <- ape::read.FASTA(get_path("mbd.fasta"))
    parameters$chain_length <- 2000
    parameters$clock_model <- "nonsense"

    expect_error(
      posterior <- est_marg_lik(
        parameters = parameters,
        alignment = alignment
      ),
      paste0(
        "'clock_model' must be among the following: ",
        paste(get_clock_models(), collapse = ", ")
      )
    )

    parameters <- open_parameters_file(get_path("parameters.csv"))
    alignment <- ape::read.FASTA(get_path("mbd.fasta"))
    parameters$chain_length <- 2000
    parameters$site_model <- "nonsense"

    expect_error(
      posterior <- est_marg_lik(
        parameters = parameters,
        alignment = alignment
      ),
      paste0(
        "'site_model' must be among the following: ",
        paste(get_site_models(), collapse = ", ")
      )
    )
  }
})
