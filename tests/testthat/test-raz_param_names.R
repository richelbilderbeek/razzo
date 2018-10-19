context("raz_create_input_files")

test_that("use", {

  testthat::expect_error(
    raz_get_param_checks(name = 'pippobaudo', 0.1)
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'lambda', -0.1)
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'mu', -0.1)
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'nu', -0.1)
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'q', 2)
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'q', -4)
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'q', 'ammaccabanane')
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'soc', 23)
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'cond', 3)
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'sequence_length', 10.5)
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'seed', 2.45)
  )
  testthat::expect_error(
    raz_get_param_checks(name = 'age', -71)
  )


})
