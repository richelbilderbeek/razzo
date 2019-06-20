context("test-create_paramses_mbd")

test_that("every row must be unique", {
  df <- create_paramses_mbd()
  expect_equal(nrow(unique(df)), nrow(df))
})
