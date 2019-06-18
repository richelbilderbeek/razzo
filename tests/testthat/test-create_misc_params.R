context("test-create_misc_params")

test_that("use", {
  expect_silent(create_misc_params())
})

test_that("abuse", {
  expect_error(create_misc_params("/local/tmp/not_peregrine_friendly"))
  expect_error(create_misc_params("/tmp/not_peregrine_friendly"))
})
