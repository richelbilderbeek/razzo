context("test-check_razzo_params")

test_that("use", {
  expect_silent(
    check_razzo_params(razzo_params = create_test_razzo_params())
  )

  expect_error(
    check_razzo_params(razzo_params = list()),
    "'mbd_params' must be an element of a 'razzo_params'"
  )
})
