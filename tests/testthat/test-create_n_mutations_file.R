context("test-create_n_mutations_file")

test_that("use", {

  # Should create 'results/'n_mutations.csv'
  filename <- create_n_mutations_file(
    project_folder_name = raztr::get_raztr_path("razzo_project")
  )

  # File should be created
  expect_true(file.exists(filename))

  # OK: filename must end with 'n_mutations.csv'
  expect_true(
    length(
      grep(
        pattern = "n_mutations\\.csv$", filename, perl = TRUE, value = TRUE
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
    create_n_mutations_file(project_folder_name = "nonsense"),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
