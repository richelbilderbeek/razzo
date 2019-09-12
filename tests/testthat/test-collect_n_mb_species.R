test_that("use", {

  if (1 == 2) {
    # Run the experiment if you can and need to
    for (file in list.files(
        get_razzo_path(
          "razzo_project"), recursive = TRUE, pattern = "parameters\\.RDa"
      )
    ) {
      run_razzo(open_parameters_file(file))
    }
  }
  df <- collect_n_mb_species(
    project_folder_name = get_razzo_path("razzo_project")
  )

  # No secondary key needed here :-)

  # Measurement
  expect_true("n_mb_events" %in% names(df))
  assertive::assert_all_are_whole_numbers(df$n_mb_events)

  # Folder names are strings, not factors
  assertive::assert_all_are_non_empty_character(df$folder)
  expect_true(!is.factor(df$folder))

  # Rows must be unique, easy when all seeds (and thus folder names)
  # are unique
  expect_equal(nrow(unique(df)), nrow(df))

  # Only the true tree will have MB events
  # Therefore, per parameter setting,
  # there will be one number of MB events.
  # As each parameter setting has one 'parameters.RDa' file,
  # the number of those files equals the number of MB event counts
  n_rows_expected <- length(
    list.files(
      get_razzo_path("razzo_project"),
      recursive = TRUE,
      pattern = "parameters.RDa"
    )
  )
  expect_equal(nrow(df), n_rows_expected)

  # Use relative folder path as the primary key
  #
  # E.g. an experiment with its parameter file at either of these locations
  #
  #   /home/richel/razzo_project/data/1/2/3/parameters.RDa
  #   C:\Giappo\razzo_project\data\1\2\3\parameters.RDa
  #
  # would get
  #
  #  data/1/2/3                                                                 # nolint this is not commented code
  #
  expect_true("folder" %in% names(df))
  # All folders are relative and start with 'data/'
  expect_true(
    all(
      !is.na(
        stringr::str_match(
          string = df$folder,
          pattern = "^data/.*"
        )[ ,1]
      )
    )
  )
})
