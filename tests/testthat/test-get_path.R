test_that("use", {

  expect_equal(
    raztr::get_raztr_path("parameters.RDa"),
    raztr::get_raztr_path("parameters.RDa")
  )
})

test_that("abuse", {

  expect_error(
    raztr::get_raztr_path("abs.ent"),
    "'filename' must be the name of a file in 'inst/extdata'"
  )
})
