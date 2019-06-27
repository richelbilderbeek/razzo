context("test-create_fig_1")

test_that("use", {

  # Should return plot
  plot <- create_fig_1(
    project_folder_name = get_razzo_path("razzo_project")
  )
  plot
  expect_true(
    all(class(plot) == c("gg", "ggplot"))
  )
})

test_that("abuse", {
  expect_error(
    create_fig_1("nonsense"),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
