test_that("every row must be unique", {
  df <- create_mbd_params_table()
  expect_equal(nrow(unique(df)), nrow(df))
})
