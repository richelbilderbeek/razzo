context("test-check_project_folder_name")

test_that("use", {

  folder <- file.path(tempfile(), "razzo_project")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  expect_silent(
    check_project_folder_name(
      project_folder_name = folder
    )
  )

  folder <- file.path(tempfile(), "raket_werper")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  expect_silent(
    check_project_folder_name(
      project_folder_name = folder
    )
  )

  folder <- file.path(tempfile(), "nonsense")
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  expect_error(
    check_project_folder_name(
      project_folder_name = folder
    ),
    "'project_folder_name' must end with 'razzo_project' or 'raket_werper'"
  )

  expect_error(
    check_project_folder_name(
      project_folder_name = "absent/razzo_project"
    ),
    "'project_folder_name' absent"
  )

})
