context("test-create_mbd_params_interval")

test_that("every row must be unique", {
  df <- create_mbd_params_interval()
  expect_equal(nrow(unique(df)), nrow(df))
})
