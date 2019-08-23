#' Create the parameters for all experiment
#' @inheritParams default_params_doc
#' @return a set of \code{razzo_params}
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_razzo_paramses <- function(
  project_folder_name,
  mbd_paramses = create_mbd_paramses(),
  twinning_params = pirouette::create_twinning_params(
    twin_tree_filename = peregrine::get_pff_tempfile(),
    twin_alignment_filename = peregrine::get_pff_tempfile(),
    twin_evidence_filename = peregrine::get_pff_tempfile()
  ),
  alignment_params = pirouette::create_alignment_params(
    root_sequence = pirouette::create_blocked_dna(length = 1000),
    mutation_rate = 0.5 / get_razzo_crown_age(),
    fasta_filename = peregrine::get_pff_tempfile(
      pattern = "alignment_",
      fileext = ".fasta"
    )
  ),
  error_measure_params = pirouette::create_error_measure_params(),
  mcmc_chain_length = beautier::create_mcmc()$chain_length
) {
  testit::assert(peregrine::is_pff(project_folder_name))
  data_folder_name <- "data"

  # Go through all biological parameters
  razzo_paramses <- list()
  # Must start at one, as the BEAST2 RNG seed must be at least one.
  index <- 1
  for (row_index in seq_along(mbd_paramses)) {
    # Extract the biological parameters and create a folder for them
    mbd_params <- mbd_paramses[[row_index]]
    parsettings_name <- paste0(
      mbd_params$lambda,
      "-",
      mbd_params$mu,
      "-",
      mbd_params$nu,
      "-",
      mbd_params$q
    )
    seed <- mbd_params$seed
    seed_folder <- file.path(
      project_folder_name,
      data_folder_name,
      parsettings_name,
      seed
    )
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
        beast2_working_dir = peregrine::get_pff_tempdir(),
        rng_seed = seed,
        overwrite = TRUE
      ),
      est_evidence_mcmc = beautier::create_nested_sampling_mcmc(
        epsilon = 10^-12
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
          beast2_working_dir = peregrine::get_pff_tempdir(),
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
          beast2_working_dir = peregrine::get_pff_tempdir(),
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
    razzo_paramses[[index]] <- razzo_params
    index <- index + 1
  }
  razzo_paramses
}
