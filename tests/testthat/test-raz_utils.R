context("raz_utils")

test_that("raz_site_models", {
  expect_true(
    length(raz_site_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(raz_site_models()) # nolint internal function
  )
})

test_that("raz_clock_models", {
  expect_true(
    length(raz_clock_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(raz_clock_models()) # nolint internal function
  )
})

test_that("raz_gen_models", {
  expect_true(
    length(raz_gen_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(raz_gen_models()) # nolint internal function
  )
})
