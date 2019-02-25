context("test-get_path")

test_that("use", {

  expect_equal(
    system.file("extdata", "parameters.RDa", package = "razzo"),
    get_path("parameters.RDa")
  )
})

test_that("abuse", {

  expect_error(
    get_path("abs.ent"),
    "'filename' must be the name of a file in 'inst/extdata'"
  )
})
