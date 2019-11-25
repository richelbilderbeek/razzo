context("test-check_misc_params")

test_that("use", {
  testthat::expect_silent(
    razzo::check_misc_params(
      razzo::create_misc_params()
    )
  )
})

test_that("abuse", {

  testthat::expect_error(
    razzo::check_misc_params(list()),
    "'tree_filename' must be an element of a 'misc_params'"
  )

  misc_params <- razzo::create_misc_params()
  misc_params$tree_filename <- "/peregrine_unfriendly"
  testthat::expect_error(
    razzo::check_misc_params(misc_params),
    "'misc_params\\$tree_filename' must be Peregrine-friendly"
  )

  misc_params <- razzo::create_misc_params()
  misc_params$tree_filename <- peregrine::get_pff_tempfile(pattern = "nonsense")
  testthat::expect_error(
    razzo::check_misc_params(misc_params),
    "'misc_params\\$tree_filename' must end with 'mbd.tree'"
  )

})
