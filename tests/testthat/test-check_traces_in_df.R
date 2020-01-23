test_that("use", {
  df <- data.frame(my_name = 3.14)
  names(df)
  expect_silent(check_traces_in_df("my_name", df))
  expect_error(check_traces_in_df("absent", df))
})
