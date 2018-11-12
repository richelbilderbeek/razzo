context("raz_check_file_exists")

test_that("use", {

  expect_silent(
    raz_check_file_exists(
      raz_get_path("parameters.csv")
    )
  )
  expect_error(
    raz_check_file_exists(
      "absent"
    ),
    "File not found. Could not find file with path 'absent'"
  )

})
