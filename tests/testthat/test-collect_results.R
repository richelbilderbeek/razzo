context("test-collect_results")

test_that("use", {

  if (1 == 2) {
    for (filename in list.files(
        get_razzo_path("razzo_project"),
        recursive = TRUE, pattern = "parameters\\.RDa",
        full.names = TRUE
      )
    ) {
      beautier::check_file_exists(filename)
      run_razzo(open_parameters_file(filename))
    }
  }
  df <- collect_results(
    project_folder_name = get_razzo_path("razzo_project"),
    include_all_nltt = TRUE
  )

  # Sub-keys
  expect_true("folder" %in% names(df))
  expect_true("tree" %in% names(df))
  expect_true(is.factor(df$tree))
  expect_true("best_or_gen" %in% names(df))
  expect_true(is.factor(df$best_or_gen))
  expect_true("ess_likelihood" %in% names(df))
  expect_true("nltt_1" %in% names(df))

  # Unsure, with the bug it is 4, so hope to get it lower after fixing it
  expect_true(all(df$ess_likelihood <= 4))

  # Rows must be unique
  expect_equal(nrow(unique(df)), nrow(df))

  skip(
    paste0(
      "This function has been used to fix #230",
      "Little need to maintain it"
    )
  )
  expect_true("site_model" %in% names(df))
  expect_true(is.factor(df$site_model))
  expect_true("clock_model" %in% names(df))
  expect_true(is.factor(df$clock_model))
  expect_true("tree_prior" %in% names(df))
  expect_true(is.factor(df$tree_prior))

})

test_that("use - no ltts", {

  if (1 == 2) {
    for (filename in list.files(
        get_razzo_path("razzo_project"),
        recursive = TRUE, pattern = "parameters\\.RDa",
        full.names = TRUE
      )
    ) {
      beautier::check_file_exists(filename)
      run_razzo(open_parameters_file(filename))
    }
  }
  df <- collect_results(
    project_folder_name = get_razzo_path("razzo_project"),
    include_all_nltt = FALSE
  )

  # Sub-keys
  expect_true("folder" %in% names(df))
  expect_true("tree" %in% names(df))
  expect_true(is.factor(df$tree))
  expect_true("best_or_gen" %in% names(df))
  expect_true(is.factor(df$best_or_gen))
  expect_true("ess_likelihood" %in% names(df))
  expect_true(!("nltt_1" %in% names(df)))

  # Unsure, with the bug it is 4, so hope to get it lower after fixing it
  expect_true(all(df$ess_likelihood <= 4))

  # Rows must be unique
  expect_equal(nrow(unique(df)), nrow(df))

  skip(
    paste0(
      "This function has been used to fix #230",
      "Little need to maintain it"
    )
  )
  expect_true("site_model" %in% names(df))
  expect_true(is.factor(df$site_model))
  expect_true("clock_model" %in% names(df))
  expect_true(is.factor(df$clock_model))
  expect_true("tree_prior" %in% names(df))
  expect_true(is.factor(df$tree_prior))

})

test_that("abuse", {
  expect_error(
    collect_esses(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
