context("test-raz_create_fig_1_file")

test_that("use", {
  skip("TODO. Issue 85, #85")

  # Should create 'results/fig_1.png'
  filename <- raz_creat_fig_1_file(
    project_folder_name = raz_get_path("razzo_project")
  )

  # File should be created
  expect_true(file.exists(filename))

  # OK: filename must end with 'fig_1.png'
  testthat::expect_true(
    length(
      grep(
        pattern = "figure_1\\.png$", filename, perl = TRUE, value = TRUE
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
  skip("TODO. Issue 85, #85")
  expect_error(
    raz_creat_fig_1_file(
      project_folder_name = "nonsense"
    ),
    "'project_folder_name' must end with 'razzo_project'"
  )
})
