test_that("use", {

  df <- razzo::collect_n_taxa(
    project_folder_name = raztr::get_raztr_path("razzo_project")
  )

  # No secondary key needed here :-)

  # Measurement
  expect_true("n_taxa" %in% names(df))
  expect_true(is.numeric(df$n_taxa))

  # Rows must be unique
  expect_equal(nrow(unique(df)), nrow(df))

  # As all trees (true, twin, posterior) have an equal amount of tips
  n_rows_expected <- length(
    list.files(
      raztr::get_raztr_path("razzo_project"),
      recursive = TRUE,
      pattern = "parameters\\.RDa"
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
  expect_true(is.character(df$folder))

})
