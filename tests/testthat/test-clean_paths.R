test_that("use", {

  if (rappdirs::app_dir()$os != "win") {
    expect_equal(
      razzo::clean_paths(c("a//b")),
      c("a/b")
    )
    expect_equal(
      razzo::clean_paths(c("a//b", "c//d")),
      c("a/b", "c/d")
    )
  }
  expect_error(razzo::clean_paths(NULL))
  expect_error(razzo::clean_paths(NA))
  expect_error(razzo::clean_paths(Inf))
})
