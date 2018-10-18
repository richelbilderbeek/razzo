context("raz_create_params")

test_that("use", {

  skip("TODO, Issue #19")
  lambda <- 0.1
  mu <- 0.2
  nu <- 0.3
  q <- 0.4
  seed <- 42
  age <- 15
  sequence_length <- 123

  params <- raz_create_params(
    lambda = lambda,
    mu = mu,
    nu = nu,
    q = q,
    seed = seed,
    age = age,
    sequence_length = sequence_length
  )

  expect_equal(params$lambda, lambda)
  expect_equal(params$mu, mu)
  expect_equal(params$nu, nu)
  expect_equal(params$q, q)
  expect_equal(params$seed, age)
  expect_equal(params$age, age)
  expect_equal(params$sequence_length, sequence_length)

})

test_that("abuse", {

  skip("TODO, Issue #19")
  lambda <- 0.1
  mu <- 0.2
  nu <- 0.3
  q <- 0.4
  seed <- 42
  age <- 15
  sequence_length <- 123

  expect_silent(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q,
      seed = seed,
      age = age,
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
      age = age,
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
      age = age,
      sequence_length = sequence_length
    ),
    "'mu' must be positive"
  )

  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = -123.456,
      q = q,
      seed = seed,
      age = age,
      sequence_length = sequence_length
    ),
    "'nu' must be positive"
  )
  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = -123.456,
      seed = seed,
      age = age,
      sequence_length = sequence_length
    ),
    "'q' must be in range [0, 1]"
  )
  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q,
      seed = "nonsense",
      age = age,
      sequence_length = sequence_length
    ),
    "'seed' must be numeric"
  )
  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q,
      seed = seed,
      age = -123.456,
      sequence_length = sequence_length
    ),
    "'age' must be positive"
  )
  expect_error(
    raz_create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q,
      seed = seed,
      age = age,
      sequence_length = -123456
    ),
    "'sequence_length' must be positive"
  )
})
