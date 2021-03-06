test_that("use, without candidate", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  experiments <- razzo::create_razzo_experiments(
    has_candidates = FALSE,
  )
  expect_equal(length(experiments), 1)
  expect_equal(
    experiments[[1]]$inference_conditions$model_type,
    "generative"
  )
})

test_that("use, with candidate", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  experiments <- razzo::create_razzo_experiments(
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

test_that("matches article", {

  if (rappdirs::app_dir()$os == "win") {
    skip("This can only run on Linux.")
  }

  folder_name <- peregrine::get_pff_tempfile()

  # Issue 242, Isssue #242
  experiments <- razzo::create_razzo_experiments(
    folder_name = folder_name,
    has_candidates = TRUE
  )

  ##############################################################################
  # First and generative experiment
  # Will be the same as 'create_razzo_gen_experiment,
  # and is also tested there
  ##############################################################################
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
    razzo::get_razzo_crown_age()
  )
  expect_equal(
    gen_exp$inference_model$mrca_prior$mrca_distr$sigma$value,
    0.0001
  )
  expect_equal(
    gen_exp$inference_model$mcmc$chain_length,
    razzo::get_razzo_mcmc_chain_length()
  )
  expect_equal(
    gen_exp$inference_model$mcmc$store_every,
    razzo::get_razzo_mcmc_store_every()
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$chain_length,
    razzo::get_razzo_mcmc_chain_length()
  )
  # gen_exp$est_evidence_mcmc
  ns_mcmc_gen <- razzo::create_razzo_ns_mcmc(
    folder_name = folder_name,
    model_type = "generative"
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$chain_length,
    ns_mcmc_gen$chain_length
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$store_every,
    ns_mcmc_gen$store_every
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$epsilon,
    ns_mcmc_gen$epsilon
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$particle_count,
    ns_mcmc_gen$particle_count
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$sub_chain_length,
    ns_mcmc_gen$sub_chain_length
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$tracelog$filename,
    ns_mcmc_gen$tracelog$filename
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$tracelog$log_every,
    ns_mcmc_gen$tracelog$log_every
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$treelog$filename,
    ns_mcmc_gen$treelog$filename
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$treelog$log_every,
    ns_mcmc_gen$treelog$log_every
  )

  ##############################################################################
  # Candidate experiment
  ##############################################################################
  cand_exps <- experiments[-1]


  # General
  for (i in seq_along(cand_exps)) {
    cand_exp <- cand_exps[[i]]

    # --------------------
    # Inference conditions
    # --------------------
    expect_equal(cand_exp$inference_conditions$model_type, "candidate")
    expect_equal(cand_exp$inference_conditions$run_if, "best_candidate")
    # Shouldn't we measure the evidence for the generative model?
    expect_equal(cand_exp$inference_conditions$do_measure_evidence, TRUE)
    # --------------------
    # Inference model
    # --------------------
    expect_equal(
      cand_exp$inference_model$mrca_prior$mrca_distr$name,
      "normal"
    )
    expect_equal(
      cand_exp$inference_model$mrca_prior$mrca_distr$mean$value,
      razzo::get_razzo_crown_age()
    )
    expect_equal(
      cand_exp$inference_model$mrca_prior$mrca_distr$sigma$value,
      0.0001
    )
    expect_equal(
      cand_exp$inference_model$mcmc$chain_length,
      razzo::get_razzo_mcmc_chain_length()
    )
    expect_equal(
      cand_exp$inference_model$mcmc$store_every,
      razzo::get_razzo_mcmc_store_every()
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$chain_length,
      razzo::get_razzo_mcmc_chain_length()
    )
    # cand_exp$est_evidence_mcmc
    ns_mcmc_cand <- razzo::create_razzo_ns_mcmc(
      folder_name = folder_name,
      model_type = "candidate",
      index = i
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$chain_length,
      ns_mcmc_cand$chain_length
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$store_every,
      ns_mcmc_cand$store_every
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$epsilon,
      ns_mcmc_cand$epsilon
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$particle_count,
      ns_mcmc_cand$particle_count
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$sub_chain_length,
      ns_mcmc_cand$sub_chain_length
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$tracelog$filename,
      ns_mcmc_cand$tracelog$filename
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$tracelog$log_every,
      ns_mcmc_cand$tracelog$log_every
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$treelog$filename,
      ns_mcmc_cand$treelog$filename
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$treelog$log_every,
      ns_mcmc_cand$treelog$log_every
    )
  }

  option <- 3
  if (option == 2) {
    # Four days ago, I tested for all 39 models.
    # This is perhaps the best biological setup,
    # but I think infeasable in practice,
    # as one simulation was still running up until this morning.
    expect_equal(39, length(cand_exps))
  }

  if (option == 3) {
    # Picking all 3 combination of simplest site models and tree priors
    #
    #  1. JC69 site model, strict clock model, Yule tree prior
    # (G. JC69 site model, strict clock model,   BD tree prior)
    #  2.  HKY site model, strict clock model, Yule tree prior
    #  3.  HKY site model, strict clock model,   BD tree prior
    expect_equal(3, length(cand_exps))
    # site models
    expect_equal(cand_exps[[1]]$inference_model$site_model$name, "JC69")
    expect_equal(cand_exps[[2]]$inference_model$site_model$name, "HKY")
    expect_equal(cand_exps[[3]]$inference_model$site_model$name, "HKY")
    # clock models
    expect_equal(cand_exps[[1]]$inference_model$clock_model$name, "strict")
    expect_equal(cand_exps[[2]]$inference_model$clock_model$name, "strict")
    expect_equal(cand_exps[[3]]$inference_model$clock_model$name, "strict")
    # tree priors
    expect_equal(cand_exps[[1]]$inference_model$tree_prior$name, "yule")
    expect_equal(cand_exps[[2]]$inference_model$tree_prior$name, "yule")
    expect_equal(cand_exps[[3]]$inference_model$tree_prior$name, "birth_death")
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
  expect_equal(
    gen_exp$beast2_options$input_filename,
    file.path(folder_name, "mbd_gen.xml")
  )
  expect_equal(
    gen_exp$inference_model$mcmc$tracelog$filename,
    file.path(folder_name, "mbd_gen.log")
  )
  expect_equal(
    gen_exp$inference_model$mcmc$treelog$filename,
    file.path(folder_name, "mbd_gen.trees")
  )
  expect_equal(
    gen_exp$beast2_options$output_state_filename,
    file.path(folder_name, "mbd_gen.xml.state")
  )
  expect_equal(
    gen_exp$errors_filename,
    file.path(folder_name, "mbd_nltts_gen.csv")
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$tracelog$filename,
    file.path(folder_name, "mbd_gen_evidence.log")
  )
  expect_equal(
    gen_exp$est_evidence_mcmc$treelog$filename,
    file.path(folder_name, "mbd_gen_evidence.trees")
  )


  # Candidate experiments
  candidate_experiments <- experiments[-1]
  for (i in seq_along(candidate_experiments)) {
    cand_exp <- candidate_experiments[[i]]
    testit::assert(cand_exp$inference_conditions$model_type == "candidate")
    expect_equal(
      cand_exp$beast2_options$input_filename,
      file.path(folder_name, "mbd_best.xml")
    )
    expect_equal(
      cand_exp$inference_model$mcmc$tracelog$filename,
      file.path(folder_name, "mbd_best.log")
    )
    expect_equal(
      cand_exp$inference_model$mcmc$treelog$filename,
      file.path(folder_name, "mbd_best.trees")
    )
    expect_equal(
      cand_exp$beast2_options$output_state_filename,
      file.path(folder_name, "mbd_best.xml.state")
    )
    expect_equal(
      cand_exp$errors_filename,
      file.path(folder_name, "mbd_nltts_best.csv")
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$tracelog$filename,
      file.path(folder_name, paste0("mbd_best_", i, "_evidence.log"))
    )
    expect_equal(
      cand_exp$est_evidence_mcmc$treelog$filename,
      file.path(folder_name, paste0("mbd_best_", i, "_evidence.trees"))
    )
  }
})
