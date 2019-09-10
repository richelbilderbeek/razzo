test_that("use", {
  expect_silent(
    collect_run_times(get_razzo_path("razzo_project"))
  )
})
