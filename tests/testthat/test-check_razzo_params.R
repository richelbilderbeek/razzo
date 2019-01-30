context("test-check_razzo_params")

test_that("abuse", {
  razzo_params <- list()
  expect_error(
    check_razzo_params(razzo_params)
  )
})
