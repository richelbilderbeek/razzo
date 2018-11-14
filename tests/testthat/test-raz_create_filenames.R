context("raz_create_filenames")

test_that("raz_create_filename_mbd_tree", {
  skip("This function is actually not used")
  expect_true(
    raz_create_filename_mbd_tree(
      parameters,
      folder_name
    )
  )
})

test_that("raz_create_filename_bd_tree", {
  skip("This function is actually not used")
  expect_true(
    raz_create_filename_bd_tree(
      parameters,
      folder_name
    )
  )
})

test_that("raz_create_filename_mbd_alignment", {
  skip("This function is actually not used")
  expect_true(
    raz_create_filename_mbd_alignment(
      parameters,
      folder_name
    )
  )
})

test_that("raz_create_filename_bd_alignment", {
  skip("This function is actually not used")
  expect_true(
    raz_create_filename_bd_alignment(
      parameters,
      folder_name
    )
  )
})
