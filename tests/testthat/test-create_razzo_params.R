context("test-create_params_razzo")

test_that("use", {

  expect_silent(
    check_razzo_params(
      create_test_razzo_params()
    )
  )

})
