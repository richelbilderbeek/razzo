test_that("use", {

  expect_equal(
    raztr::get_raztr_path("parameters.RDa"),
    get_razzo_path("parameters.RDa")
  )
})

test_that("abuse", {

  expect_error(
    get_razzo_path("abs.ent"),
    "'filename' must be the name of a file in 'inst/extdata'"
  )
})
