context("test-check_file_exists")

test_that("use", {

  expect_silent(
    check_file_exists(
      get_path("parameters.RDa")
    )
  )
  expect_error(
    check_file_exists(
      "absent"
    ),
    "File not found. Could not find file with path 'absent'"
  )

})
