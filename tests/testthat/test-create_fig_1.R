context("test-create_fig_1")

test_that("use", {
  skip("WIP")
  # Should return plot
  plot <- create_fig_1(
    project_folder_name = get_path("razzo_project")
  )
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
