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
  # Issue 242, Isssue #242
  pir_params <- create_razzo_pir_params(
    has_candidates = TRUE,
    has_twinning = TRUE
  )

  ##############################################################################
  # pir_params$twinning_params
  ##############################################################################
  expect_true(!beautier::is_one_na(pir_params$twinning_params))
  expect_equal(pir_params$twinning_params$twin_model, "birth_death")
  expect_equal(pir_params$twinning_params$method, "random_tree")
  ##############################################################################
  # pir_params$alignment_params
  ##############################################################################
  expect_equal(
    pirouette::create_blocked_dna(length = 1000),
    pir_params$alignment_params$root_sequence
  )
  expect_equal(
    pirouette::create_standard_mutation_rate,
    pir_params$alignment_params$mutation_rate
  )
  expect_equal(
    pir_params$alignment_params$site_model$name,
    "JC69"
  )
  expect_equal(
    pir_params$alignment_params$clock_model$name,
    "strict"
  )

  ##############################################################################
  # pir_params$experiments
  ##############################################################################
  gen_exp <- pir_params$experiments[[1]]
  gen_exp$inference_conditions
  gen_exp$inference_model
  gen_exp$beast2_options
  gen_exp$est_evidence_mcmc

  ##############################################################################
  # pir_params$error_measure_params
  ##############################################################################

  # Etcetera ...
})
