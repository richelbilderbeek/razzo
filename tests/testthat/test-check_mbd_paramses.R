test_that("use", {
  expect_silent(
    check_mbd_paramses(
      create_mbd_paramses()))
})

test_that("no duplicates", {
  good_mbd_paramses <- create_mbd_paramses()
  expect_silent(check_mbd_paramses(good_mbd_paramses))

  mbd_paramses <- good_mbd_paramses
  mbd_paramses[[1]] <- mbd_paramses[[2]]
  expect_error(
    check_mbd_paramses(mbd_paramses),
    "All mbd_params in mbd_paramses must be unique"
  )
})
