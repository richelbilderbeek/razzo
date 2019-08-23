test_that("use", {
  razzo_paramses <- create_test_razzo_paramses()
  expect_equal(length(razzo_paramses), 2)
})
