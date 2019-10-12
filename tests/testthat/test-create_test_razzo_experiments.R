test_that("use, without candidates", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  experiments <- create_test_razzo_experiments(
    has_candidates = FALSE,
  )
  expect_equal(length(experiments), 1)
  expect_equal(
    experiments[[1]]$inference_conditions$model_type,
    "generative"
  )
})

test_that("use, with candidates", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  experiments <- create_test_razzo_experiments(
    has_candidates = TRUE
  )
  expect_true(length(experiments) > 1)
  expect_equal(
    experiments[[1]]$inference_conditions$model_type,
    "generative"
  )
  expect_equal(
    experiments[[2]]$inference_conditions$model_type,
    "candidate"
  )
})

test_that("values", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  # Issue 242, Isssue #242
  experiments <- create_test_razzo_experiments(
    has_candidates = TRUE
  )
  # Generative and two candidates
  expect_equal(length(experiments), 3)

  ##############################################################################
  # experiments
  ##############################################################################
  # First and generative experiment
  gen_exp <- experiments[[1]]
  # --------------------
  # Inference conditions
  # --------------------
  expect_equal(gen_exp$inference_conditions$model_type, "generative")
  expect_equal(gen_exp$inference_conditions$run_if, "always")
  # Shouldn't we measure the evidence for the generative model?
  expect_equal(gen_exp$inference_conditions$do_measure_evidence, TRUE)
  # --------------------
  # Inference model
  # --------------------
  expect_equal(gen_exp$inference_model$site_model$name, "JC69")
  expect_equal(gen_exp$inference_model$clock_model$name, "strict")
  expect_equal(gen_exp$inference_model$tree_prior$name, "birth_death")
  expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$name,
    "normal"
  )
  expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$mean$value,
    get_razzo_crown_age()
  )
  expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$sigma$value,
    0.0001
  )
  expect_equal(
    gen_exp$inference_model$mcmc$chain_length,
    3e3
  )
  expect_equal(
    gen_exp$inference_model$mcmc$store_every,
    1e3
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$chain_length,
    3e3
  )
  # gen_exp$est_evidence_mcmc
  expect_equal(
    gen_exp$est_evidence_mcmc$chain_length,
    3e3
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$store_every,
    1e3
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$epsilon,
    create_razzo_nested_sampling_mcmc()$epsilon
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$particle_count,
    create_razzo_nested_sampling_mcmc()$particle_count
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$sub_chain_length,
    create_razzo_nested_sampling_mcmc()$sub_chain_length
  )
})

test_that("razzo naming conventions", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  folder_name <- peregrine::get_pff_tempfile()
  experiments <- create_test_razzo_experiments(
    folder_name = folder_name,
    has_candidates = TRUE
  )
  # Generative experiment
  gen_exp <- experiments[[1]]
  testit::assert(gen_exp$inference_conditions$model_type == "generative")
  expect_equal(
    gen_exp$beast2_options$input_filename,
    file.path(folder_name, "mbd_gen.xml")
  )
  expect_equal(
    gen_exp$beast2_options$output_log_filename,
    file.path(folder_name, "mbd_gen.log")
  )
  expect_equal(
    gen_exp$beast2_options$output_trees_filenames,
    file.path(folder_name, "mbd_gen.trees")
  )
  expect_equal(
    gen_exp$beast2_options$output_state_filename,
    file.path(folder_name, "mbd_gen.xml.state")
  )
  expect_true(peregrine::is_pff(gen_exp$beast2_options$beast2_working_dir))
  expect_equal(
    gen_exp$errors_filename,
    file.path(folder_name, "mbd_nltts_gen.csv")
  )
  # Candidate experiments
  for (cand_exp in experiments[-1]) {
    testit::assert(cand_exp$inference_conditions$model_type == "candidate")
    expect_equal(
      cand_exp$beast2_options$input_filename,
      file.path(folder_name, "mbd_best.xml")
    )
    expect_equal(
      cand_exp$beast2_options$output_log_filename,
      file.path(folder_name, "mbd_best.log")
    )
    expect_equal(
      cand_exp$beast2_options$output_trees_filenames,
      file.path(folder_name, "mbd_best.trees")
    )
    expect_equal(
      cand_exp$beast2_options$output_state_filename,
      file.path(folder_name, "mbd_best.xml.state")
    )
    expect_true(peregrine::is_pff(cand_exp$beast2_options$beast2_working_dir))
    expect_equal(
      cand_exp$errors_filename,
      file.path(folder_name, "mbd_nltts_best.csv")
    )
  }
})
