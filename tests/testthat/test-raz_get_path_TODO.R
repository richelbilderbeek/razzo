context("raz_get_path")

test_that("use", {

  # TODO: Issue #6: razzo must provide testing resources
  if (1 == 2) {
    expect_equal(
      system.file("extdata", "parameters.csv", package = "razzo"),
      raz_get_path("parameters.csv")
    )
  }
})

test_that("abuse", {

  # TODO: Issue #6: razzo must provide testing resources
  if (1 == 2) {
    expect_error(
      raz_get_path("abs.ent"),
      "'filename' must be the name of a file in 'inst/extdata'"
    )
  }
})
