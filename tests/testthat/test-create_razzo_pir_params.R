test_that("use", {
  ##############################################################################
  # FALSE FALSE
  ##############################################################################
  pir_params <- create_razzo_pir_params(
    has_candidates = FALSE,
    has_twinning = FALSE
  )
  expect_equal(length(pir_params$experiments), 1)
  expect_equal(
    pir_params$experiments[[1]]$inference_conditions$model_type,
    "generative"
  )
  expect_true(beautier::is_one_na(pir_params$twinning_params))

  ##############################################################################
  # FALSE TRUE
  ##############################################################################
  pir_params <- create_razzo_pir_params(
    has_candidates = FALSE,
    has_twinning = TRUE
  )
  expect_equal(length(pir_params$experiments), 1)
  expect_equal(
    pir_params$experiments[[1]]$inference_conditions$model_type,
    "generative"
  )
  expect_false(beautier::is_one_na(pir_params$twinning_params))

  ##############################################################################
  # TRUE FALSE
  ##############################################################################
  pir_params <- create_razzo_pir_params(
    has_candidates = TRUE,
    has_twinning = FALSE
  )
  expect_true(length(pir_params$experiments) > 1)
  expect_equal(
    pir_params$experiments[[1]]$inference_conditions$model_type,
    "generative"
  )
  expect_equal(
    pir_params$experiments[[2]]$inference_conditions$model_type,
    "candidate"
  )
  expect_true(beautier::is_one_na(pir_params$twinning_params))

  ##############################################################################
  # TRUE TRUE
  ##############################################################################
  pir_params <- create_razzo_pir_params(
    has_candidates = TRUE,
    has_twinning = TRUE
  )
  expect_true(length(pir_params$experiments) > 1)
  expect_equal(
    pir_params$experiments[[1]]$inference_conditions$model_type,
    "generative"
  )
  expect_equal(
    pir_params$experiments[[2]]$inference_conditions$model_type,
    "candidate"
  )
  expect_false(beautier::is_one_na(pir_params$twinning_params))
})

test_that("matches article", {
  pir_params <- create_razzo_pir_params(
    has_candidates = TRUE,
    has_twinning = TRUE
  )
  # We use twinning
  expect_true(!beautier::is_one_na(pir_params$twinning_params))

  # Etcetera ...
})
