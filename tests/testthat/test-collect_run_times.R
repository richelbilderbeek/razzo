test_that("use", {
  expect_silent(
    collect_run_times(raztr::get_raztr_path("razzo_project"))
  )
})
