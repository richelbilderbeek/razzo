context("raz_create_params")

test_that("use", {

  lambda <- 0.1
  mu <- 0.2
  nu <- 0.3
  q <- 0.4
  seed <- 42
  crown_age <- 15
  sequence_length <- 123

  params <- raz_create_params(
    lambda = lambda,
    mu = mu,
    nu = nu,
    q = q,
    seed = seed,
    crown_age = crown_age,
    sequence_length = sequence_length
  )

  expect_equal(params$lambda, lambda)
  expect_equal(params$mu, mu)
  expect_equal(params$nu, nu)
  expect_equal(params$q, q)
  expect_equal(params$seed, seed)
  expect_equal(params$crown_age, crown_age)
  expect_equal(params$sequence_length, sequence_length)

})

test_that("abuse", {

  lambda <- 0.1
  mu <- 0.2
  nu <- 0.3
  q <- 0.4
  seed <- 42
  crown_age <- 15
  sequence_length <- 123

  expect_silent(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q,
      seed = seed,
      crown_age = crown_age,
      sequence_length = sequence_length
    )
  )

  expect_error(
    raz_create_params(
      lambda = -123.456,
      mu = mu,
      nu = nu,
      q = q,
      seed = seed,
      crown_age = crown_age,
      sequence_length = sequence_length
    ),
    "'lambda' must be positive"
  )

  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = -123.456,
      nu = nu,
      q = q,
      seed = seed,
      crown_age = crown_age,
      sequence_length = sequence_length
    ),
    "mu has to be non negative"
  )

  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = -123.456,
      q = q,
      seed = seed,
      crown_age = crown_age,
      sequence_length = sequence_length
    ),
    "nu has to be non negative"
  )
  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = -123.456,
      seed = seed,
      crown_age = crown_age,
      sequence_length = sequence_length
    ),
    "q has to be between zero and one"
  )
  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q,
      seed = "nonsense",
      crown_age = crown_age,
      sequence_length = sequence_length
    ),
    "seed must be an integer"
  )
  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q,
      seed = seed,
      crown_age = -123.456,
      sequence_length = sequence_length
    ),
    "age has to be non negative"
  )
  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q,
      seed = seed,
      crown_age = crown_age,
      sequence_length = -123456
    ),
    "sequence_length has to be a positive integer number"
  )
})
