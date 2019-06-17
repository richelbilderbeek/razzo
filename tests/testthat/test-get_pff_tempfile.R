test_that("use", {
  # Twice, to be sure all files are unique
  expect_silent(get_pff_tempfile())
  expect_silent(get_pff_tempfile())
})

test_that("file must not exist", {
  filename <- get_pff_tempfile()
  expect_false(file.exists(filename))

  filename <- get_pff_tempfile()
  expect_false(file.exists(filename))
})

test_that("must be Peregrine frienldy", {
  expect_true(is_pff(get_pff_tempfile()))
})
