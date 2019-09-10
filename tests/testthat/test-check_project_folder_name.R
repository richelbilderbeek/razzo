context("test-check_project_folder_name")

test_that("use", {

  project_folder_name <- file.path(tempfile(), "razzo_project")
  dir.create(project_folder_name, recursive = TRUE, showWarnings = FALSE)
  expect_silent(
    check_project_folder_name(project_folder_name)
  )

  project_folder_name <- file.path(tempfile(), "razzo_project_12345678")
  dir.create(project_folder_name, recursive = TRUE, showWarnings = FALSE)
  expect_silent(
    check_project_folder_name(project_folder_name)
  )

  project_folder_name <- file.path(tempfile(), "raket_werper")
  dir.create(project_folder_name, recursive = TRUE, showWarnings = FALSE)
  expect_silent(
    check_project_folder_name(project_folder_name)
  )

  project_folder_name <- file.path(tempfile(), "nonsense")
  dir.create(project_folder_name, recursive = TRUE, showWarnings = FALSE)
  expect_error(
    check_project_folder_name(project_folder_name),
    "'project_folder_name' must end with 'razzo_project' or 'raket_werper'"
  )

  expect_error(
    check_project_folder_name(
      project_folder_name = "absent/razzo_project"
    ),
    "'project_folder_name' absent"
  )

})
