context("test-raz_create_nltt_stats_file")

test_that("use", {
  skip("TODO. Issue 88, #88")

  # Should create 'results/nltt_stats.csv'
  filename <- raz_create_nltt_stats_file(
    project_folder_name = raz_get_path("razzo_project")
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
  skip("TODO. Issue 88, #88")
  expect_error(
    raz_create_nltt_stats_file(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
