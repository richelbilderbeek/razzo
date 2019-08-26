context("test-create_misc_params")

test_that("use", {
  expect_silent(create_misc_params())
})

test_that("abuse", {
  expect_error(create_misc_params("/not_peregrine_friendly"))
})

test_that("matches article", {
  misc_params <- create_misc_params()
})

test_that("follows naming convention", {
  folder_name <- peregrine::get_pff_tempfile()
  misc_params <- create_misc_params(folder_name)
  expect_equal(
    misc_params$tree_filename,
    file.path(folder_name, "mbd.tree")
  )
})
