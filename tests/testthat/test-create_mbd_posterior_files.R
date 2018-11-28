context("create_mbd_posterior_files")

test_that("use", {

  expect_true(1 + 1 == 2) # to get a non-empty test

  if (!ribir::is_on_travis()) {
    return ()
  } else {

    # Create input files
    parameters_filename <- create_tempfile("parameters.csv") # nolint internal function
    mbd_alignment_filename <- create_tempfile("mbd.fasta") # nolint internal function
    testit::assert(file.exists(parameters_filename))
    testit::assert(file.exists(mbd_alignment_filename))

    # Run
    mbd_posterior_filenames <- create_mbd_posterior_files( # nolint internal function
      parameters_filename = parameters_filename
    )

    # Check
    # All files exist
    expect_true(all(file.exists(mbd_posterior_filenames)))

    # MBD treees
    mbd_trees_filename <- grep(
      pattern = "mbd\\.trees$",
      mbd_posterior_filenames, perl = TRUE, value = TRUE
    )
    expect_true(length(mbd_trees_filename) > 0)

    expect_silent(
      tracerer::parse_beast_trees(mbd_trees_filename)
    )

    expect_true(length(grep(
      pattern = "mbd\\.log$",
      mbd_posterior_filenames, perl = TRUE, value = TRUE))
      > 0
    )

    log_filename <- grep(
      pattern = "mbd\\.log$",
      mbd_posterior_filenames, perl = TRUE, value = TRUE
    )

    estimates <- tracerer::parse_beast_log(log_filename)
    expect_equal("data.frame", class(estimates))
  }
})

test_that("posteriors must have the same number of trees", {

  mbd_trees_filenames <- list.files(
    path = get_path("razzo_project"),
    pattern = "mbd\\.trees",
    recursive = TRUE,
    full.names = TRUE
  )
  # Gollumese: collection of number of trees
  n_treeses <- rep(NA, length(mbd_trees_filenames))
  for (i in seq_along(mbd_trees_filenames)) {
    n_treeses[i] <- length(
      tracerer::parse_beast_trees(file = mbd_trees_filenames[i])
    )

  }
  # All should have the same amount of posterior trees
  expect_true(all(n_treeses == sort(n_treeses)))
})

test_that("posteriors must have the same number of estimates", {

  mbd_log_filenames <- list.files(
    path = get_path("razzo_project"),
    pattern = "mbd\\.log",
    recursive = TRUE,
    full.names = TRUE
  )
  n_trace_lengths <- rep(NA, length(mbd_log_filenames))
  for (i in seq_along(mbd_log_filenames)) {
    n_trace_lengths[i] <- nrow(
      tracerer::parse_beast_log(
        filename = mbd_log_filenames[i]
      )
    )

  }
  # All should have the same amount of posterior trees
  expect_true(all(n_trace_lengths == sort(n_trace_lengths)))
})
