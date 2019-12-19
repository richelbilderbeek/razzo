test_that("use", {

  if (1 == 2) {
    # Run the experiment if you can and need to
    for (file in list.files(
        razzo::get_razzo_path(
          "razzo_project"), recursive = TRUE, pattern = "parameters\\.RDa"
      )
    ) {
      razzo::run_razzo(razzo::open_parameters_file(file))
    }
  }
  df <- razzo::collect_n_mb_species(
    project_folder_name = razzo::get_razzo_path("razzo_project")
  )

  # No secondary key needed here :-)

  # Measurements
  testthat::expect_true("n_mb_species" %in% names(df))
  testthat::expect_true("f_mb_species" %in% names(df))
  assertive::assert_all_are_whole_numbers(df$n_mb_species)
  assertive::assert_is_numeric(df$f_mb_species)
  testthat::expect_true(all(df$f_mb_species >= 0.0))
  testthat::expect_true(all(df$f_mb_species <= 1.0))

  # Folder names are strings, not factors
  assertive::assert_all_are_non_empty_character(df$folder)
  testthat::expect_true(!is.factor(df$folder))

  # Rows must be unique, easy when all seeds (and thus folder names)
  # are unique
  testthat::expect_equal(nrow(unique(df)), nrow(df))

  # Only the true tree will have MB events
  # Therefore, per parameter setting,
  # there will be one number of MB events.
  # As each parameter setting has one 'parameters.RDa' file,
  # the number of those files equals the number of MB event counts
  n_rows_expected <- length(
    list.files(
      razzo::get_razzo_path("razzo_project"),
      recursive = TRUE,
      pattern = "parameters.RDa"
    )
  )
  testthat::expect_equal(nrow(df), n_rows_expected)

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
  testthat::expect_true("folder" %in% names(df))
  # All folders are relative and start with 'data/'
  testthat::expect_true(
    all(
      !is.na(
        stringr::str_match(
          string = df$folder,
          pattern = "^data/.*"
        )[, 1]
      )
    )
  )
})
