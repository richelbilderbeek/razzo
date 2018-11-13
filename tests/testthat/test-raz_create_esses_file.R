context("test-raz_create_esses_file")

test_that("use", {
  skip("TODO. Issue 87, #87")

  # Should create 'results/esses.csv'
  filename <- raz_create_esses_file(
    project_folder_name = raz_get_path("razzo_project")
  )

  # File should be created
  expect_true(file.exists(filename))

  # OK: filename must end with 'esses.csv'
  testthat::expect_true(
    length(
      grep(
        pattern = "esses\\.csv$", filename, perl = TRUE, value = TRUE
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
  skip("TODO. Issue 87, #87")
  expect_error(
    raz_create_esses_file(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
