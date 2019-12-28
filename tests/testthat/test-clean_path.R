test_that("use", {
  if (rappdirs::app_dir()$os != "win") {
    expect_equal(razzo::clean_path(""), "")
    expect_equal(razzo::clean_path("a"), "a")
    expect_equal(razzo::clean_path("a/b"), "a/b")
    expect_equal(razzo::clean_path("a/b/"), "a/b/")
    expect_equal(razzo::clean_path("a//b"), "a/b")
    expect_equal(razzo::clean_path("a//b//c"), "a/b/c")
  }
  expect_error(razzo::clean_path(NULL))
  expect_error(razzo::clean_path(NA))
  expect_error(razzo::clean_path(Inf))
})
