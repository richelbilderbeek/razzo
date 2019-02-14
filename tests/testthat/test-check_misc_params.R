context("test-check_misc_params")

test_that("use", {
  expect_silent(
    check_misc_params(create_misc_params())
  )
})

test_that("abuse", {
  misc_params <- list()
  misc_params$pippo <- "mbd.tree"
  expect_error(
    check_misc_params(misc_params),
    "'tree_filename' must be an element of a 'misc_params'"
  )
})
