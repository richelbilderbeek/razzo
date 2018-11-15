context("raz_get_settings_paths")

test_that("use", {
  all_paths <- raz_get_settings_paths(raz_get_path("razzo_project"))
  expect_true(
    length(all_paths) > 0
  )
  expect_true(
    length(all_paths) %%
      (length(raz_get_site_models()) * length(raz_get_clock_models)) == 0 # nolint internal function
  )
  expect_true(
   is.character(all_paths)
  )
})
