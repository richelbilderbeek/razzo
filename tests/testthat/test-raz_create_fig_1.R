context("test-raz_create_fig_1")

test_that("use", {
  # Should return plot
  plot <- raz_create_fig_1(
    project_folder_name = raz_get_path("razzo_project")
  )
  expect_true(
    all(class(plot) == c("gg", "ggplot"))
  )
})

test_that("abuse", {
  expect_error(
    raz_create_fig_1("nonsense"),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
