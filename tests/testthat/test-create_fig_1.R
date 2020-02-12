test_that("use", {

  plots <- razzo::create_fig_1(
    project_folder_name = raztr::get_raztr_path("razzo_project")
  )
  testthat::expect_equal(class(plots), "list")

  if (is.list(plots)) {
    for (i in seq_along(plots)) {
      testthat::expect_true(
        all(class(plots[[i]]) == c("gg", "ggplot"))
      )
    }
  } else {
    testthat::expect_true(
      all(class(plots) == c("gg", "ggplot"))
    )
  }
})

test_that("abuse", {
  testthat::expect_error(
    razzo::create_fig_1("nonsense"),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
