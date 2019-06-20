context("test-get_results_path")

test_that("use", {
  skip("Add results first")
  results_path <- get_results_path(get_razzo_path("razzo_project"))
  expect_true(
    length(results_path) > 0
  )
  expect_true(
   is.character(results_path)
  )
  expect_true(
   grepl(pattern = "results", x = results_path)
  )
})
