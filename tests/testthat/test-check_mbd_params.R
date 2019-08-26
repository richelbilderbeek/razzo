test_that("use", {

  expect_silent(
    check_mbd_params(create_test_mbd_params())
  )
})

test_that("abuse", {

  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = "a",
        mu = 0.15,
        nu = 0.2,
        q = 0.01,
        crown_age = 15.0,
        cond = 1,
        seed = 1
      )
    ),
    "'lambda' must be numeric"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = "a",
        nu = 0.2,
        q = 0.01,
        crown_age = 15.0,
        cond = 1,
        seed = 1
      )
    ),
    "'mu' must be numeric"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = 0.15,
        nu = "a",
        q = 0.01,
        crown_age = 15.0,
        cond = 1,
        seed = 1
      )
    ),
    "'nu' must be numeric"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = 0.15,
        nu = 0.2,
        q = "a",
        crown_age = 15.0,
        cond = 1,
        seed = 1
      )
    ),
    "'q' must be numeric"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = -1,
        mu = 0.15,
        nu = 0.2,
        q = 0.01,
        crown_age = 15.0,
        cond = 1,
        seed = 1
      )
    ),
    "'lambda' must be positive"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = -1,
        nu = 0.2,
        q = 0.01,
        crown_age = 15.0,
        cond = 1,
        seed = 1
      )
    ),
    "'mu' must be positive"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = 0.15,
        nu = -1,
        q = 0.01,
        crown_age = 15.0,
        cond = 1,
        seed = 1
      )
    ),
    "'nu' must be positive"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = 0.15,
        nu = 0.2,
        q = -1,
        crown_age = 15.0,
        cond = 1,
        seed = 1
      )
    ),
    "'q' must be positive"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = 0.15,
        nu = 0.2,
        q = 2,
        crown_age = 15.0,
        cond = 1,
        seed = 1
      )
    ),
    "'q' must be less or equal to 1.0"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = 0.15,
        nu = 0.2,
        q = 0.1,
        crown_age = 15.0,
        cond = "a",
        seed = 1
      )
    ),
    "'cond' must be numeric"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = 0.15,
        nu = 0.2,
        q = 0.1,
        crown_age = -15.0,
        cond = 1,
        seed = 1
      )
    ),
    "'crown_age' must be positive"
  )
  expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = 0.15,
        nu = 0.2,
        q = 0.1,
        crown_age = 15.0,
        cond = 1,
        seed = "a"
      )
    ),
    "'seed' must be integer or NA"
  )
    expect_error(
    check_mbd_params(
      mbd_params = create_mbd_params(
        lambda = 0.2,
        mu = 0.15,
        nu = 0.2,
        q = 0.1,
        crown_age = 15.0,
        cond = 1,
        seed = 3.5
      )
    ),
    "'seed' must be integer or NA"
  )
})
