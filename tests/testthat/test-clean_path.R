test_that("use", {
  if (rappdirs::app_dir()$os != "win") {
    testthat::expect_equal(razzo::clean_path(""), "")
    testthat::expect_equal(razzo::clean_path("a"), "a")
    testthat::expect_equal(razzo::clean_path("a/b"), "a/b")
    testthat::expect_equal(razzo::clean_path("a/b/"), "a/b/")
    testthat::expect_equal(razzo::clean_path("a//b"), "a/b")
    testthat::expect_equal(razzo::clean_path("a//b//c"), "a/b/c")
  }
  testthat::expect_error(razzo::clean_path(NULL))
  testthat::expect_error(razzo::clean_path(NA))
  testthat::expect_error(razzo::clean_path(Inf))
})
