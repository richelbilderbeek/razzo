context("test-collect_n_mutations")

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
  df <- razzo::collect_n_mutations(
    project_folder_name = razzo::get_razzo_path("razzo_project")
  )

  # No secondary key needed here :-)

  # Measurement
  expect_true("n_mutations" %in% names(df))
  expect_true(is.numeric(df$n_mutations))

  # Rows must be unique
  expect_equal(nrow(unique(df)), nrow(df))

  # As all trees (true, twin, posterior) have an equal amount of tips
  n_rows_expected <- length(
    list.files(
      razzo::get_razzo_path("razzo_project"),
      recursive = TRUE,
      pattern = "*.fasta"
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

})
