test_that("use, without candidate", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  experiments <- razzo::create_razzo_experiments(
    has_candidates = FALSE,
  )
  testthat::expect_equal(length(experiments), 1)
  testthat::expect_equal(
    experiments[[1]]$inference_conditions$model_type,
    "generative"
  )
})

test_that("use, with candidate", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  experiments <- create_razzo_experiments(
    has_candidates = TRUE
  )
  testthat::expect_true(length(experiments) > 1)
  testthat::expect_equal(
    experiments[[1]]$inference_conditions$model_type,
    "generative"
  )
  testthat::expect_equal(
    experiments[[2]]$inference_conditions$model_type,
    "candidate"
  )
})

test_that("matches article", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  # Issue 242, Isssue #242
  experiments <- razzo::create_razzo_experiments(
    has_candidates = TRUE
  )

  ##############################################################################
  # First and generative experiment
  ##############################################################################
  gen_exp <- experiments[[1]]
  # --------------------
  # Inference conditions
  # --------------------
  testthat::expect_equal(gen_exp$inference_conditions$model_type, "generative")
  testthat::expect_equal(gen_exp$inference_conditions$run_if, "always")
  # Shouldn't we measure the evidence for the generative model?
  testthat::expect_equal(gen_exp$inference_conditions$do_measure_evidence, TRUE)
  # --------------------
  # Inference model
  # --------------------
  testthat::expect_equal(gen_exp$inference_model$site_model$name, "JC69")
  testthat::expect_equal(gen_exp$inference_model$clock_model$name, "strict")
  testthat::expect_equal(gen_exp$inference_model$tree_prior$name, "birth_death")
  testthat::expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$name,
    "normal"
  )
  testthat::expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$mean$value,
    razzo::get_razzo_crown_age()
  )
  testthat::expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$sigma$value,
    0.0001
  )
  testthat::expect_equal(
    gen_exp$inference_model$mcmc$chain_length,
    razzo::get_razzo_mcmc_chain_length()
  )
  testthat::expect_equal(
    gen_exp$inference_model$mcmc$store_every,
    razzo::get_razzo_mcmc_store_every()
  )
  testthat::expect_equal(
    gen_exp$est_evidence_mcmc$chain_length,
    razzo::get_razzo_mcmc_chain_length()
  )
  # gen_exp$est_evidence_mcmc
  testthat::expect_equal(
    gen_exp$est_evidence_mcmc$chain_length,
    razzo::create_razzo_nested_sampling_mcmc()$chain_length
  )
  testthat::expect_equal(
    gen_exp$est_evidence_mcmc$store_every,
    razzo::create_razzo_nested_sampling_mcmc()$store_every
  )
  testthat::expect_equal(
    gen_exp$est_evidence_mcmc$epsilon,
    razzo::create_razzo_nested_sampling_mcmc()$epsilon
  )
  testthat::expect_equal(
    gen_exp$est_evidence_mcmc$particle_count,
    razzo::create_razzo_nested_sampling_mcmc()$particle_count
  )
  testthat::expect_equal(
    gen_exp$est_evidence_mcmc$sub_chain_length,
    razzo::create_razzo_nested_sampling_mcmc()$sub_chain_length
  )

  ##############################################################################
  # Candidate experiment
  ##############################################################################
  cand_exps <- experiments[-1]


  # General
  for (cand_exp in cand_exps) {
    # --------------------
    # Inference conditions
    # --------------------
    testthat::expect_equal(cand_exp$inference_conditions$model_type, "candidate")
    testthat::expect_equal(cand_exp$inference_conditions$run_if, "best_candidate")
    # Shouldn't we measure the evidence for the generative model?
    testthat::expect_equal(cand_exp$inference_conditions$do_measure_evidence, TRUE)
    # --------------------
    # Inference model
    # --------------------
    testthat::expect_equal(
      cand_exp$inference_model$mrca_prior$mrca_distr$name,
      "normal"
    )
    testthat::expect_equal(
      cand_exp$inference_model$mrca_prior$mrca_distr$mean$value,
      razzo::get_razzo_crown_age()
    )
    testthat::expect_equal(
      cand_exp$inference_model$mrca_prior$mrca_distr$sigma$value,
      0.0001
    )
    testthat::expect_equal(
      cand_exp$inference_model$mcmc$chain_length,
      razzo::get_razzo_mcmc_chain_length()
    )
    testthat::expect_equal(
      cand_exp$inference_model$mcmc$store_every,
      razzo::get_razzo_mcmc_store_every()
    )
    testthat::expect_equal(
      cand_exp$est_evidence_mcmc$chain_length,
      razzo::get_razzo_mcmc_chain_length()
    )
    # cand_exp$est_evidence_mcmc
    testthat::expect_equal(
      cand_exp$est_evidence_mcmc$chain_length,
      razzo::create_razzo_nested_sampling_mcmc()$chain_length
    )
    testthat::expect_equal(
      cand_exp$est_evidence_mcmc$store_every,
      razzo::create_razzo_nested_sampling_mcmc()$store_every
    )
    testthat::expect_equal(
      cand_exp$est_evidence_mcmc$epsilon,
      razzo::create_razzo_nested_sampling_mcmc()$epsilon
    )
    testthat::expect_equal(
      cand_exp$est_evidence_mcmc$particle_count,
      razzo::create_razzo_nested_sampling_mcmc()$particle_count
    )
    testthat::expect_equal(
      cand_exp$est_evidence_mcmc$sub_chain_length,
      razzo::create_razzo_nested_sampling_mcmc()$sub_chain_length
    )
  }

  option <- 3
  if (option == 2) {
    # Four days ago, I tested for all 39 models.
    # This is perhaps the best biological setup,
    # but I think infeasable in practice,
    # as one simulation was still running up until this morning.
    testthat::expect_equal(39, length(cand_exps))
  }

  if (option == 3) {
    # Picking all 3 combination of simplest site models and tree priors
    #
    #  1. JC69 site model, strict clock model, Yule tree prior
    # (G. JC69 site model, strict clock model,   BD tree prior)
    #  2.  HKY site model, strict clock model, Yule tree prior
    #  3.  HKY site model, strict clock model,   BD tree prior
    testthat::expect_equal(3, length(cand_exps))
    # site models
    testthat::expect_equal(cand_exps[[1]]$inference_model$site_model$name, "JC69")
    testthat::expect_equal(cand_exps[[2]]$inference_model$site_model$name, "HKY")
    testthat::expect_equal(cand_exps[[3]]$inference_model$site_model$name, "HKY")
    # clock models
    testthat::expect_equal(cand_exps[[1]]$inference_model$clock_model$name, "strict")
    testthat::expect_equal(cand_exps[[2]]$inference_model$clock_model$name, "strict")
    testthat::expect_equal(cand_exps[[3]]$inference_model$clock_model$name, "strict")
    # tree priors
    testthat::expect_equal(cand_exps[[1]]$inference_model$tree_prior$name, "yule")
    testthat::expect_equal(cand_exps[[2]]$inference_model$tree_prior$name, "yule")
    testthat::expect_equal(cand_exps[[3]]$inference_model$tree_prior$name, "birth_death")
  }
})

test_that("razzo naming conventions", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  folder_name <- peregrine::get_pff_tempfile()
  experiments <- razzo::create_razzo_experiments(
    folder_name = folder_name,
    has_candidates = TRUE
  )
  # Generative experiment
  gen_exp <- experiments[[1]]
  testit::assert(gen_exp$inference_conditions$model_type == "generative")
  testthat::expect_equal(
    gen_exp$beast2_options$input_filename,
    file.path(folder_name, "mbd_gen.xml")
  )
  testthat::expect_equal(
    gen_exp$inference_model$mcmc$tracelog$filename,
    file.path(folder_name, "mbd_gen.log")
  )
  testthat::expect_equal(
    gen_exp$inference_model$mcmc$treelog$filename,
    file.path(folder_name, "mbd_gen.trees")
  )
  testthat::expect_equal(
    gen_exp$beast2_options$output_state_filename,
    file.path(folder_name, "mbd_gen.xml.state")
  )
  testthat::expect_equal(
    gen_exp$errors_filename,
    file.path(folder_name, "mbd_nltts_gen.csv")
  )
  # Candidate experiments
  for (cand_exp in experiments[-1]) {
    testit::assert(cand_exp$inference_conditions$model_type == "candidate")
    testthat::expect_equal(
      cand_exp$beast2_options$input_filename,
      file.path(folder_name, "mbd_best.xml")
    )
    testthat::expect_equal(
      cand_exp$inference_model$mcmc$tracelog$filename,
      file.path(folder_name, "mbd_best.log")
    )
    testthat::expect_equal(
      cand_exp$inference_model$mcmc$treelog$filename,
      file.path(folder_name, "mbd_best.trees")
    )
    testthat::expect_equal(
      cand_exp$beast2_options$output_state_filename,
      file.path(folder_name, "mbd_best.xml.state")
    )
    testthat::expect_equal(
      cand_exp$errors_filename,
      file.path(folder_name, "mbd_nltts_best.csv")
    )
  }
})
