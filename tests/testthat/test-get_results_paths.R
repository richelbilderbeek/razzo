context("get_results_path")

test_that("use", {
  results_path <- get_results_path(get_path("razzo_project"))
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
