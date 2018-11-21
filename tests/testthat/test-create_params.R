context("create_params")

test_that("use", {

  lambda <- 0.1
  mu <- 0.2
  nu <- 0.3
  q <- 0.4
  seed <- 42
  crown_age <- 15
  sequence_length <- 123
  clock_model <- "strict"
  site_model <- "jc69"

  params <- create_params(
    lambda = lambda,
    mu = mu,
    nu = nu,
    q = q,
    seed = seed,
    crown_age = crown_age,
    sequence_length = sequence_length,
    clock_model = clock_model,
    site_model = site_model
  )

  expect_equal(params$lambda, lambda)
  expect_equal(params$mu, mu)
  expect_equal(params$nu, nu)
  expect_equal(params$q, q)
  expect_equal(params$seed, seed)
  expect_equal(params$crown_age, crown_age)
  expect_equal(params$sequence_length, sequence_length)
  expect_equal(params$clock_model, clock_model)
  expect_equal(params$site_model, site_model)

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
    create_params(
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
    create_params(
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
    create_params(
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
    create_params(
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
    create_params(
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
    create_params(
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
    create_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q,
      seed = seed,
      crown_age = -123.456,
      sequence_length = sequence_length
    ),
    "age has to be positive"
  )
  expect_error(
    create_params(
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

test_that("clock model", {

  # Construct params with strict clock model
  expect_silent(
    create_params(
      clock_model = "strict"
    )
  )

  # Construct params with relaxed log-normal clock model
  expect_silent(
    create_params(
      clock_model = "rln"
    )
  )

  # Nonsense clock model
  expect_error(
    create_params(
      clock_model = "nonsense"
    ),
    paste0(
      "'clock_model' must be among the following: ",
      paste(get_clock_models(), collapse = ", ")
    )
  )

  # Retrieve strict clock from parameters
  expect_equal(
    "strict",
    create_params(clock_model = "strict")$clock_model
  )

  # Retrieve strict clock from parameters
  expect_equal(
    "rln",
    create_params(clock_model = "rln")$clock_model
  )

})

test_that("site model", {

  # Construct params with JC69 site model
  expect_silent(
    create_params(
      site_model = "jc69"
    )
  )

  # Construct params with GTR site model
  expect_silent(
    create_params(
      site_model = "gtr"
    )
  )

  # Nonsense site model
  expect_error(
    create_params(
      site_model = "nonsense"
    ),
    paste0(
      "'site_model' must be among the following: ",
      paste(get_site_models(), collapse = ", ")
    )
  )

  # Retrieve JC69 site model from parameters
  expect_equal(
    "jc69",
    create_params(site_model = "jc69")$site_model
  )

  # Retrieve GTR site model from parameters
  expect_equal(
    "gtr",
    create_params(site_model = "gtr")$site_model
  )

})
