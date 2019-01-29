context("create_posterior")

test_that("use", {

  expect_true(1 + 1 == 2) # to get a non-empty test

  if (!beastier::is_on_travis()) {
    return()
  } else {

    parameters <- open_parameters_file(get_path("parameters.csv"))
    alignment <- ape::read.FASTA(get_path("mbd.fasta"))
    parameters$chain_length <- 10000

    posterior <- create_posterior(
      parameters = parameters,
      alignment = alignment
    )
    expect_true("trees" %in% names(posterior))
    expect_true("estimates" %in% names(posterior))
    expect_true(
      class(posterior$trees) == "multiPhylo"
    )
    expect_true(
      class(posterior$trees[[1]]) == "phylo"
    )
    expect_true(
      max(posterior$estimates$Sample) == parameters$chain_length # nolint internal function
    )
    expect_true(
      all(posterior$estimates$likelihood <= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$treeLikelihood <= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$TreeHeight >= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$BDBirthRate >= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$BDDeathRate >= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$clockRate > 0) # nolint internal function
    )
    expect_true(
      !is.null(posterior$operators) # nolint internal function
    )

    parameters <- open_parameters_file(get_path("parameters.csv"))
    alignment <- ape::read.FASTA(get_path("mbd.fasta"))
    parameters$chain_length <- 2000
    parameters$clock_model <- "rln"

    posterior <- create_posterior(
      parameters = parameters,
      alignment = alignment
    )
    expect_true("trees" %in% names(posterior))
    expect_true("estimates" %in% names(posterior))
    expect_true(
      class(posterior$trees) == "multiPhylo"
    )
    expect_true(
      class(posterior$trees[[1]]) == "phylo"
    )
    expect_true(
      max(posterior$estimates$Sample) == parameters$chain_length # nolint internal function
    )
    expect_true(
      all(posterior$estimates$likelihood <= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$treeLikelihood <= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$TreeHeight >= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$BDBirthRate >= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$BDDeathRate >= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$clockRate > 0) # nolint internal function
    )
    expect_true(
      !is.null(posterior$operators) # nolint internal function
    )

    parameters <- open_parameters_file(get_path("parameters.csv"))
    alignment <- ape::read.FASTA(get_path("mbd.fasta"))
    parameters$chain_length <- 2000
    parameters$site_model <- "gtr"

    posterior <- create_posterior(
      parameters = parameters,
      alignment = alignment
    )
    expect_true("trees" %in% names(posterior))
    expect_true("estimates" %in% names(posterior))
    expect_true(
      class(posterior$trees) == "multiPhylo"
    )
    expect_true(
      class(posterior$trees[[1]]) == "phylo"
    )
    expect_true(
      max(posterior$estimates$Sample) == parameters$chain_length # nolint internal function
    )
    expect_true(
      all(posterior$estimates$likelihood <= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$treeLikelihood <= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$TreeHeight >= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$BDBirthRate >= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$BDDeathRate >= 0) # nolint internal function
    )
    expect_true(
      all(posterior$estimates$clockRate > 0) # nolint internal function
    )
    expect_true(
      !is.null(posterior$operators) # nolint internal function
    )
  }
})

test_that("abuse", {

  expect_true(1 + 1 == 2) # to get a non-empty test

  if (!beastier::is_on_travis()) {
    return()
  } else {

    parameters <- open_parameters_file(get_path("parameters.csv"))
    alignment <- ape::read.FASTA(get_path("mbd.fasta"))
    parameters$chain_length <- 2000
    parameters$clock_model <- "nonsense"

    expect_error(
      posterior <- create_posterior(
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
      posterior <- create_posterior(
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
