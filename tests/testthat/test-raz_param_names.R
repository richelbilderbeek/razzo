context("raz_create_parameters")

test_that("wrong parameters should return errors", {

  testthat::expect_error(
    raz_get_param_checks(name = "pippobaudo", 0.1)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "lambda", -0.1)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "mu", -0.1)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "nu", -0.1)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "q", 2)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "q", -4)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "q", "ammaccabanane")
  )
  testthat::expect_error(
    raz_get_param_checks(name = "soc", 23)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "cond", 3)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "sequence_length", 10.5)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "seed", 2.45)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "age", -71)
  )
  testthat::expect_error(
    raz_get_param_checks(name = "mbd_mutation_rate", -71)
  )

})

test_that("specific functions do the same thing as the generic function", {

  testthat::expect_equal(
    razzo::raz_create_param(name = "lambda", value = 1),
    razzo::raz_create_param_lambda(value = 1)
  )
  testthat::expect_equal(
    razzo::raz_create_param(name = "mu", value = 1),
    razzo::raz_create_param_mu(value = 1)
  )
  testthat::expect_equal(
    razzo::raz_create_param(name = "nu", value = 1),
    razzo::raz_create_param_nu(value = 1)
  )
  testthat::expect_equal(
    razzo::raz_create_param(name = "q", value = 0.5),
    razzo::raz_create_param_q(value = 0.5)
  )
  testthat::expect_equal(
    razzo::raz_create_param(name = "seed", value = 1),
    razzo::raz_create_param_seed(value = 1)
  )
  testthat::expect_equal(
    razzo::raz_create_param(name = "cond", value = 1),
    razzo::raz_create_param_cond(value = 1)
  )
  testthat::expect_equal(
    razzo::raz_create_param(name = "age", value = 1),
    razzo::raz_create_param_age(value = 1)
  )
  testthat::expect_equal(
    razzo::raz_create_param(name = "soc", value = 1),
    razzo::raz_create_param_soc(value = 1)
  )
  testthat::expect_equal(
    razzo::raz_create_param(name = "sequence_length", value = 1),
    razzo::raz_create_param_sequence_length(value = 1)
  )
  testthat::expect_equal(
    razzo::raz_create_param(name = "mbd_mutation_rate", value = 1),
    razzo::raz_create_param_mbd_mutation_rate(value = 1)
  )

})
