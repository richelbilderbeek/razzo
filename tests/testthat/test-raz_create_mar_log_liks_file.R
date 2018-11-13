context("test-raz_create_mar_log_liks_file")

test_that("use", {
  skip("TODO. Issue 86, #86")

  # Should create 'results/'mar_log_liks.csv'
  filename <- create_mar_log_liks_file(raz_get_path("razzo_project"))

  # File should be created
  expect_true(file.exists(filename))

  # OK: filename must end with 'mar_log_liks.csv'
  testthat::expect_true(
    length(
      grep(
        pattern = "mar_log_liks\\.csv$", filename, perl = TRUE, value = TRUE
      )
    ) > 0
  )
  # OK: should be in razzo_project/results folder
  # Use ..? to indicate one or two back- or normal slashes
  testthat::expect_true(
    length(
      grep(
        pattern = "razzo_project..?results..?",
        filename, perl = TRUE, value = TRUE
      )
    ) > 0
  )

})

test_that("abuse", {
  skip("TODO. Issue 86, #86")
  expect_error(
    create_mar_log_liks_file("nonsense"),
    "'folder_name' must end with 'razzo_project'"
  )
})
