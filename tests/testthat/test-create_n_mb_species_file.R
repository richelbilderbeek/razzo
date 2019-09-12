test_that("use", {

  # Should create 'results/'marg_liks.csv'
  filename <- create_n_mb_species_file(
    project_folder_name = get_razzo_path("razzo_project")
  )

  # File should be created
  expect_true(file.exists(filename))

  # OK: filename must end with 'n_mb_species.csv'
  expect_true(
    length(
      grep(
        pattern = "n_mb_species\\.csv$", filename, perl = TRUE, value = TRUE
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
    create_n_mb_species_file(project_folder_name = "nonsense"),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
