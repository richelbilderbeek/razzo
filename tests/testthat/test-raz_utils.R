context("raz_utils")

test_that("raz_get_site_models", {
  expect_true(
    length(raz_get_site_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(raz_get_site_models()) # nolint internal function
  )
})

test_that("raz_get_clock_models", {
  expect_true(
    length(raz_get_clock_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(raz_get_clock_models()) # nolint internal function
  )
})

test_that("raz_get_gen_models", {
  expect_true(
    length(raz_get_gen_models()) > 0 # nolint internal function
  )
  expect_true(
    is.character(raz_get_gen_models()) # nolint internal function
  )
})
