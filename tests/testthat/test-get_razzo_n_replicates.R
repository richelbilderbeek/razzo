test_that("use", {
  expect_true(is.numeric(get_razzo_n_replicates()))
  expect_true(get_razzo_n_replicates() %% 1 == 0)
  expect_true(get_razzo_n_replicates() > 0)
})
