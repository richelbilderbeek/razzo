context("test-check_project_folder_name")

test_that("use", {

  expect_error(
    raz_create_fig_1_file(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )

  expect_error(
    raz_create_fig_1_file(
      project_folder_name = "absent/razzo_project"
    ),
    "'project_folder_name' absent"
  )

})
