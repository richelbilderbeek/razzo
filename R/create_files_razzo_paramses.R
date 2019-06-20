#' Create all parameter files  in
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @aliases create_parameters_files create_files_razzo_paramses
#' @export create_parameters_files create_files_razzo_paramses
create_parameters_files <- create_files_razzo_paramses <- function(
  project_folder_name = getwd(),
  experiment_type = "test"
) {
  testit::assert(is_pff(project_folder_name))
  testit::assert(experiment_type == "test" || experiment_type == "full")
  if (experiment_type == "test") {
    n_replicates <- 2
    mbd_paramses <- razzo::create_paramses_mbd(
      lambda = 0.2,
      mu = 0.15,
      nu = 1.0,
      q = 0.1,
      seed = seq(from = 1, to = n_replicates, by = 1),
      crown_age = 6.0,
      cond = 1
    )
    testit::assert(
      nrow(unique(mbd_paramses)) == nrow(mbd_paramses)
    )
    parameters_filenames <- save_razzo_paramses(
      project_folder_name = project_folder_name,
      mbd_paramses = mbd_paramses,
      mcmc_chain_length = 3000
    )
    testit::assert(nrow(mbd_paramses) == length(parameters_filenames))
  } else {
    n_replicates <- 10
    mbd_paramses <- razzo::create_paramses_mbd(
      lambda = 0.2,
      mu = c(0, 0.15),
      nu = c(1.0, 1.5, 2.0, 2.5),
      q = c(0.1, 0.15, 0.2),
      seed = seq(from = 1, to = n_replicates, by = 1),
      crown_age = 6.0,
      cond = 1
    )
    testit::assert(
      nrow(unique(mbd_paramses)) == nrow(mbd_paramses)
    )
    parameters_filenames <- save_razzo_paramses(
      project_folder_name = project_folder_name,
      mbd_paramses = mbd_paramses,
      mcmc_chain_length = 3000
    )
    testit::assert(nrow(mbd_paramses) == length(parameters_filenames))
  }
  parameters_filenames
}

#' Create the parameter files according to the specified arguments
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
save_razzo_paramses <- function(
  project_folder_name,
  mbd_paramses = razzo::create_paramses_mbd(
    lambda = 0.2,
    mu = 0.15,
    nu = c(1.0, 1.5, 2.0),
    q = c(0.1, 0.15, 0.2),
    seed = seq(from = 1, to = 2, by = 1),
    crown_age = 15.0,
    cond = 1
  ),
  twinning_params = pirouette::create_twinning_params(
    twin_tree_filename = get_pff_tempfile(),
    twin_alignment_filename = get_pff_tempfile(),
    twin_evidence_filename = get_pff_tempfile()
  ),
  alignment_params = pirouette::create_alignment_params(
    root_sequence = "aaaaccccggggttt",
    mutation_rate = 0.5 / unique(mbd_paramses$crown_age),
    fasta_filename = get_pff_tempfile(
      pattern = "alignment_",
      fileext = ".fasta"
    )
  ),
  error_measure_params = pirouette::create_error_measure_params(),
  mcmc_chain_length = beautier::create_mcmc()$chain_length / 100
) {
  testit::assert(is_pff(project_folder_name))
  # Must start at one, as the BEAST2 RNG seed must be at least one.
  index <- 1

  # Create data folder
  data_folder_name <- "data"
  dir.create(
    file.path(project_folder_name, data_folder_name),
    showWarnings = FALSE
  )
  testit::assert(dir.exists(file.path(project_folder_name, data_folder_name)))

  # Go through all biological parameters
  n_rows <- nrow(mbd_paramses)
  parameters_filenames <- rep(NA, n_rows)
  for (row_index in seq(1:n_rows)) {
    # Extract the biological parameters and create a folder for them
    mbd_params <- mbd_paramses[row_index, ]
    parsettings_name <- paste0(
      mbd_params$lambda,
      "-",
      mbd_params$mu,
      "-",
      mbd_params$nu,
      "-",
      mbd_params$q
    )
    dir.create(
      file.path(
        project_folder_name,
        data_folder_name,
        parsettings_name
      ),
      showWarnings = FALSE
    )
    seed <- mbd_params$seed
    seed_folder <- file.path(
      project_folder_name,
      data_folder_name,
      parsettings_name,
      seed
    )
    dir.create(file.path(seed_folder), showWarnings = FALSE)
    alignment_params$fasta_filename <- file.path(
      seed_folder, "mbd.fasta"
    )
    twinning_params$twin_tree_filename <- file.path(
      seed_folder, "mbd_twin.tree"
    )
    twinning_params$twin_alignment_filename <- file.path(
      seed_folder, "mbd_twin.fasta"
    )
    twinning_params$twin_evidence_filename <- file.path(
      seed_folder, "mbd_marg_lik_twin.csv"
    )
    misc_params <- razzo::create_misc_params()
    misc_params$tree_filename <- file.path(
      seed_folder, "mbd.tree"
    )
    check_misc_params(misc_params)
    mrca_prior <- beautier::create_mrca_prior(
      is_monophyletic = TRUE,
      mrca_distr = beautier::create_normal_distr(
        mean = mbd_params$crown_age,
        sigma = 0.0001
      )
    )

    mcmc <- beautier::create_mcmc(chain_length = mcmc_chain_length, store_every = 1000)

    # name                |model_type | run_if         | measure  | inference # nolint this is no commented code
    #                     |           |                | evidence | model     # nolint this is no commented code
    # --------------------|-----------|----------------|----------|---------- # nolint this is no commented code
    # experiment_jc69_bd  |generative | always         |TRUE      |JC69, BD   # nolint this is no commented code
    # experiment_jc69_yule|candidate  | best_candidate |TRUE      |JC69, Yule # nolint this is no commented code
    # experiment_gtr_bd   |candidate  | best_candidate |TRUE      |GTR, BD    # nolint this is no commented code
    #
    # Sure, a fourth model (gtr_yule) would finish the pattern,
    # but this would also needlessly slow down our tests

    if (rappdirs::app_dir()$os != "win") {
      do_measure_evidence <- TRUE
    } else {
      do_measure_evidence <- FALSE
    }

    experiment_jc69_bd <- pirouette::create_experiment(
      inference_conditions = pirouette::create_inference_conditions(
        model_type = "generative",
        run_if = "always",
        do_measure_evidence = do_measure_evidence
      ),
      inference_model = beautier::create_inference_model(
        site_model = beautier::create_jc69_site_model(),
        tree_prior = beautier::create_bd_tree_prior(),
        mcmc = mcmc,
        mrca_prior = mrca_prior
      ),
      beast2_options = beastier::create_beast2_options(
        input_filename = file.path(seed_folder, "mbd_gen.xml"),
        output_log_filename = file.path(seed_folder, "mbd_gen.log"),
        output_trees_filenames = file.path(seed_folder, "mbd_gen.trees"),
        output_state_filename = file.path(seed_folder, "mbd_gen.xml.state"),
        beast2_working_dir = get_pff_tempdir(),
        rng_seed = seed,
        overwrite = TRUE
      ),
      est_evidence_mcmc = beautier::create_nested_sampling_mcmc(
        epsilon = 100.0
      ),
      errors_filename = file.path(seed_folder, "mbd_nltts_gen.csv")
    )
    if (rappdirs::app_dir()$os != "win") {
      experiment_jc69_yule <- pirouette::create_experiment(
        inference_conditions = pirouette::create_inference_conditions(
          model_type = "candidate",
          run_if = "best_candidate",
          do_measure_evidence = do_measure_evidence
        ),
        inference_model = beautier::create_inference_model(
          site_model = beautier::create_jc69_site_model(),
          tree_prior = beautier::create_yule_tree_prior(),
          mcmc = mcmc,
          mrca_prior = mrca_prior
        ),
        beast2_options = beastier::create_beast2_options(
          input_filename = file.path(seed_folder, "mbd_best.xml"),
          output_log_filename = file.path(seed_folder, "mbd_best.log"),
          output_trees_filenames = file.path(seed_folder, "mbd_best.trees"),
          output_state_filename = file.path(seed_folder, "mbd_best.xml.state"),
          beast2_working_dir = get_pff_tempdir(),
          rng_seed = seed,
          overwrite = TRUE
        ),
        est_evidence_mcmc = beautier::create_nested_sampling_mcmc(
          epsilon = 100.0
        ),
        errors_filename = file.path(seed_folder, "mbd_nltts_best.csv")
      )
      experiment_gtr_bd <- pirouette::create_experiment(
        inference_conditions = pirouette::create_inference_conditions(
          model_type = "candidate",
          run_if = "best_candidate",
          do_measure_evidence = do_measure_evidence
        ),
        inference_model = beautier::create_inference_model(
          site_model = beautier::create_gtr_site_model(),
          tree_prior = beautier::create_bd_tree_prior(),
          mcmc = mcmc,
          mrca_prior = mrca_prior
        ),
        beast2_options = beastier::create_beast2_options(
          input_filename = file.path(seed_folder, "mbd_best.xml"),
          output_log_filename = file.path(seed_folder, "mbd_best.log"),
          output_trees_filenames = file.path(seed_folder, "mbd_best.trees"),
          output_state_filename = file.path(seed_folder, "mbd_best.xml.state"),
          beast2_working_dir = get_pff_tempdir(),
          rng_seed = seed,
          overwrite = TRUE
        ),
        est_evidence_mcmc = beautier::create_nested_sampling_mcmc(
          epsilon = 100.0
        ),
        errors_filename = file.path(seed_folder, "mbd_nltts_best.csv")
      )
      experiments <- list(
        experiment_jc69_bd, # generative
        experiment_jc69_yule, # candidate
        experiment_gtr_bd # candidate
      )
    } else {
      experiments <- list(experiment_jc69_bd)
    }

    pir_params <- pirouette::create_pir_params(
      alignment_params = alignment_params,
      twinning_params = twinning_params,
      experiments = experiments,
      error_measure_params = error_measure_params,
      evidence_filename = file.path(seed_folder, "mbd_marg_lik.csv")
    )
    razzo_params <- razzo::create_razzo_params(
      mbd_params = mbd_params,
      pir_params = pir_params,
      misc_params = misc_params
    )
    check_razzo_params(razzo_params)
    parameters_filenames[index] <- file.path(
      seed_folder,
      "parameters.RDa"
    )
    saveRDS(object = razzo_params, file = parameters_filenames[index])
    testthat::expect_silent(
      check_razzo_params(readRDS(parameters_filenames[index]))
    )
    index <- index + 1
  }
  parameters_filenames
}
