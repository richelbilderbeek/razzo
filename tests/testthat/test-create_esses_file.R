test_that("use", {
  # Works locally
  if (beastier::is_on_travis()) return()

  # Should create 'results/esses.csv'
  filename <- create_esses_file(
    project_folder_name = raztr::get_raztr_path("razzo_project")
  )

  # File should be created
  expect_true(file.exists(filename))

  # OK: filename must end with 'esses.csv'
  expect_true(
    length(
      grep(
        pattern = "esses\\.csv$", filename, perl = TRUE, value = TRUE
      )
    ) > 0
  )
  # OK: should be in razzo_project/results folder
  # Use ..? to indicate one or two back- or normal slashes
  expect_true(
    length(
      grep(
        pattern = "razzo_project..?results..?",
        filename, perl = TRUE, value = TRUE
      )
    ) > 0
  )

})

test_that("abuse", {
  expect_error(
    create_esses_file(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
