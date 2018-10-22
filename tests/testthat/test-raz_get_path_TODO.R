context("raz_get_path")

test_that("use", {

  expect_equal(
    system.file("extdata", "parameters.csv", package = "razzo"),
    raz_get_path("parameters.csv")
  )
})

test_that("abuse", {

  expect_error(
    raz_get_path("abs.ent"),
    "'filename' must be the name of a file in 'inst/extdata'"
  )
})
