context("test-get_pff_tempdir")

test_that("use", {
  expect_true(is_pff(get_pff_tempdir()))
})
