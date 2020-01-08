test_that("use", {

  # Should create 'results/fig_1.png'
  filename <- create_fig_1_file(
    project_folder_name = get_razzo_path("razzo_project")
  )

  # File should be created
  expect_true(all(file.exists(filename)))

  # OK: filename must end with 'figure_1.png'
  expect_true(
    all(
      grep(
      pattern = "figure_1", filename
    ) &
      grep(
        pattern = ".png", filename
      )
    )
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
    create_fig_1_file(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
