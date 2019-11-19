test_that("use", {

  if (rappdirs::app_dir()$os != "win") {
    testthat::expect_equal(
      razzo::clean_paths(c("a//b")),
      c("a/b")
    )
    testthat::expect_equal(
      razzo::clean_paths(c("a//b", "c//d")),
      c("a/b", "c/d")
    )
  }
  testthat::expect_error(razzo::clean_paths(NULL))
  testthat::expect_error(razzo::clean_paths(NA))
  testthat::expect_error(razzo::clean_paths(Inf))
})
