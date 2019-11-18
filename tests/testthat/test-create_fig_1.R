context("test-create_fig_1")

test_that("use", {

  skip("Cannot read parameters from files yet")
  # Should return plot
  plots <- create_fig_1(
    project_folder_name = get_razzo_path("razzo_project")
  )
  if (is.list(plots)) {
    for (i in seq_along(plots)) {
      expect_true(
        all(class(plots[[i]]) == c("gg", "ggplot"))
      )
    }
  } else {
    expect_true(
      all(class(plots) == c("gg", "ggplot"))
    )
  }
})

test_that("abuse", {
  expect_error(
    create_fig_1("nonsense"),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
