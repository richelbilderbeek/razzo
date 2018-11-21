context("get_data_paths")

test_that("use", {
  all_paths <- get_data_paths(get_path("razzo_project"))
  expect_true(
    length(all_paths) > 0
  )
  expect_true(
    length(all_paths) %%
      (length(get_site_models()) * length(get_clock_models)) == 0 # nolint internal function
  )
  expect_true(
   is.character(all_paths)
  )
})
