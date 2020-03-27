test_that("Package style", {
  lintr::expect_lint_free(relative_path = FALSE)
})
