test_that("use", {

  if (rappdirs::app_dir()$os != "win") {
    expect_equal(
      clean_paths(c("a//b")),
      c("a/b")
    )
    expect_equal(
      clean_paths(c("a//b", "c//d")),
      c("a/b", "c/d")
    )
  }
  expect_error(clean_paths(NULL))
  expect_error(clean_paths(NA))
  expect_error(clean_paths(Inf))
})
