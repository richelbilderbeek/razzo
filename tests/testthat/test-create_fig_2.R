context("test-create_fig_2")

test_that("use", {
  skip("https://github.com/richelbilderbeek/pirouette/issues/145")
  # Should return plot
  plot <- create_fig_2(
    project_folder_name = get_path("razzo_project")
  )
  expect_true(
    all(class(plot) == c("gg", "ggplot"))
  )
})

test_that("abuse", {
  expect_error(
    create_fig_2("nonsense"),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
