context("test-create_marg_liks_file")

test_that("use", {

  # Should create 'results/'marg_liks.csv'
  filename <- create_marg_liks_file(
    project_folder_name = get_path("razzo_project")
  )

  # File should be created
  expect_true(file.exists(filename))

  # OK: filename must end with 'marg_liks.csv'
  testthat::expect_true(
    length(
      grep(
        pattern = "marg_liks\\.csv$", filename, perl = TRUE, value = TRUE
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
  expect_error(
    create_marg_liks_file(project_folder_name = "nonsense"),
    "'project_folder_name' must end with 'razzo_project'"
  )
})

test_that("marginal likelihood estimates must differ", {

  skip("TODO: Issue 145, #145")
  first_params_file_path <- list.files(
    path = get_path("razzo_project"),
    pattern = "parameters.csv",
    recursive = TRUE,
    full.names = TRUE
  )[1]

  mbd_marg_lik_file <- file.path(
    dirname(first_params_file_path),
    "mbd_marg_lik.csv"
  )
  bd_marg_lik_file <- file.path(
    dirname(first_params_file_path),
    "bd_marg_lik.csv"
  )
  testit::assert(file.exists(mbd_marg_lik_file))
  testit::assert(file.exists(bd_marg_lik_file))
  mbd_marg_lik <- utils::read.csv(mbd_marg_lik_file)$marg_log_lik
  bd_marg_lik <- utils::read.csv(bd_marg_lik_file)$marg_log_lik
  expect_true(mbd_marg_lik != bd_marg_lik)
})
