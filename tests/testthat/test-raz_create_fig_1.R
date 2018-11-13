context("test-raz_create_fig_1")

test_that("use", {
  skip("TODO. Issue 79, #79")

  # Should return a list of two plots
  plots <- raz_creat_fig_1(raz_get_path("razzo_project"))
  expect_equal("list", class(plots))
  expect_true("bd" %in% names(plots))
  expect_true("mbd" %in% names(plots))
})

test_that("abuse", {
  skip("TODO. Issue 79, #79")
  expect_error(
    raz_creat_fig_1("nonsense"),
    "'folder_name' must end with 'razzo_project'"
  )
})
