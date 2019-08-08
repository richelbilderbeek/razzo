context("test-get_results_path")

test_that("use", {

  project_folder_name <- file.path(tempfile(), "razzo_project")
  # No warning if folder already exists
  dir.create(project_folder_name, recursive = TRUE, showWarnings = FALSE)

  # Simply adds the 'results' foldername
  expect_equal(
    get_results_path(project_folder_name),
    file.path(project_folder_name, "results")
  )

  skip("dirty path")
  expect_equal(
    get_results_path(paste0(project_folder_name, "/")),
    file.path(project_folder_name, "results")
  )

})
