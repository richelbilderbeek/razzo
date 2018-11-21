context("test-create_nltt_stats_file")

test_that("use", {

  # Should create 'results/nltt_stats.csv'
  filename <- create_nltt_stats_file(
    project_folder_name = get_path("razzo_project")
  )

  # File should be created
  expect_true(file.exists(filename))

  # OK: filename must end with 'nltt_stats.csv'
  testthat::expect_true(
    length(
      grep(
        pattern = "nltt_stats\\.csv$", filename, perl = TRUE, value = TRUE
      )
    ) > 0
  )
  # OK: should be in razzo_project/results folder
  # Use ..? to indicate one or two back- or normal slashes
  testthat::expect_true(
    length(
      grep(
        pattern = "razzo_project..?results..?",
        filename, perl = TRUE, value = TRUE
      )
    ) > 0
  )

})

test_that("abuse", {
  expect_error(
    create_nltt_stats_file(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
