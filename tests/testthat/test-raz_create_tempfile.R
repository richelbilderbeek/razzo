context("raz_create_tempfile")

test_that("use", {

  filename <- raz_create_tempfile("parameters.csv")
  expect_true(file.exists(filename))
})

test_that("abuse", {

  expect_error(
    raz_create_tempfile("abs.ent"),
    "'filename' must be the name of a file in 'inst/extdata'"
  )
})
