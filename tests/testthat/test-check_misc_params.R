context("test-check_misc_params")

test_that("use", {
  expect_silent(check_misc_params(create_misc_params()))
})

test_that("abuse", {

  good_misc_params <- create_misc_params()
  expect_silent(check_misc_params(good_misc_params))

  # tree_filename
  misc_params <- good_misc_params
  misc_params$tree_filename <- NULL
  expect_error(
    check_misc_params(misc_params),
    "'tree_filename' must be an element of a 'misc_params'"
  )

  # razzo_version
  misc_params <- good_misc_params
  misc_params$razzo_version <- NULL
  expect_error(
    check_misc_params(misc_params),
    "'razzo_version' must be an element of a 'misc_params'"
  )

})

test_that("abuse", {

  misc_params <- create_misc_params()
  misc_params$tree_filename <- "/peregrine_unfriendly"
  expect_error(
    check_misc_params(misc_params),
    "'misc_params\\$tree_filename' must be Peregrine-friendly"
  )

  misc_params <- create_misc_params()
  misc_params$tree_filename <- peregrine::get_pff_tempfile(pattern = "nonsense")
  expect_error(
    check_misc_params(misc_params),
    "'misc_params\\$tree_filename' must end with 'mbd.tree'"
  )

})
