context("get_data_paths")

test_that("use", {

  if (!beastier::is_on_travis()) return()

  all_paths <- get_data_paths(get_path("razzo_project"))
  expect_true(
    length(all_paths) > 0
  )
  expect_true(
    is.character(all_paths)
  )

  parameter_files <- file.path(all_paths, "parameters.RDa")
  expect_true(all(file.exists(parameter_files)))
})
