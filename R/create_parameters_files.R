#' Create all parameter files  in
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_parameters_files <- function(
  project_folder_name = getwd(),
  experiment_type = "test"
) {
  testit::assert(experiment_type == "test" || experiment_type == "full")
  if (experiment_type == "test") {
    n_replicates <- 2
    # Create MBD params witha different seed per replicate
    mbd_params_interval <- create_mbd_params_interval(
      lambda = 0.2,
      mu = 0.15,
      nu = 1.0,
      q = 0.1,
      seed = seq(from = 1, to = n_replicates, by = 1),
      crown_age = 15.0,
      cond = 1
    )
    testit::assert(
      nrow(unique(mbd_params_interval))
      == nrow(mbd_params_interval)
    )
    parameters_filenames <- create_full_parameters_files(
      project_folder_name = project_folder_name,
      mbd_params_interval = mbd_params_interval
    )
    testit::assert(nrow(mbd_params_interval) == length(parameters_filenames))
  } else {
    n_replicates <- 10
    mbd_params_interval <- create_mbd_params_interval(
      lambda = 0.2,
      mu = 0.15,
      nu = c(1.0, 1.5, 2.0),
      q = c(0.1, 0.15, 0.2),
      seed = seq(from = 1, to = n_replicates, by = 1),
      crown_age = 15.0,
      cond = 1
    )
    testit::assert(nrow(unique(mbd_params_interval))
      == nrow(mbd_params_interval)
    )
    parameters_filenames <- create_full_parameters_files(
      project_folder_name = project_folder_name,
      mbd_params_interval = mbd_params_interval
    )
    testit::assert(nrow(mbd_params_interval) == length(parameters_filenames))
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
create_full_parameters_files <- function(
  project_folder_name = getwd(),
  mbd_params_interval = create_mbd_params_interval(
    lambda = 0.2,
    mu = 0.15,
    nu = c(1.0, 1.5, 2.0),
    q = c(0.1, 0.15, 0.2),
    seed = seq(from = 1, to = 2, by = 1),
    crown_age = 15.0,
    cond = 1
  ),
  twinning_params = pirouette::create_twinning_params(),
  alignment_params = pirouette::create_alignment_params(
    root_sequence = "aaaaccccggggttt",
    mutation_rate = 0.5 / unique(mbd_params_interval$crown_age)
  ),
  error_measure_params = pirouette::create_error_measure_params()
) {
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
  n_rows <- nrow(mbd_params_interval)
  parameters_filenames <- rep(NA, n_rows)
  for (row_index in seq(1:n_rows)) {
    # Extract the biological parameters and create a folder for them
    mbd_params <- mbd_params_interval[row_index, ]
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
    misc_params <- list()
    misc_params$tree_filename <- "mbd.tree"
    mrca_prior <- beautier::create_mrca_prior(
      is_monophyletic = TRUE,
      mrca_distr = beautier::create_normal_distr(mean = 15.0, sigma = 0.0001)
    )

    mcmc <- beautier::create_mcmc(chain_length = 3000, store_every = 1000)
    mrca_prior <- beautier::create_mrca_prior(
      is_monophyletic = TRUE,
      mrca_distr = beautier::create_normal_distr(mean = 15.0, sigma = 0.0001)
    )
    # name                |model_type | run_if         | measure  | inference  # nolint this is no commented code
    #                     |           |                | evidence | model
    # --------------------|-----------|----------------|----------|-----------
    # experiment_jc69_bd  |generative | always         |TRUE      |JC69, BD   # nolint this is no commented code
    # experiment_jc69_yule|candidate  | best_candidate |TRUE      |JC69, Yule # nolint this is no commented code
    # experiment_gtr_bd   |candidate  | best_candidate |TRUE      |GTR, BD    # nolint this is no commented code
    #
    # Sure, a fourth model (gtr_yule) would finish the pattern,
    # but this would also needlessly slow down our tests
    experiment_jc69_bd <- pirouette::create_experiment(
      model_type = "generative",
      run_if = "always",
      do_measure_evidence = TRUE,
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
        rng_seed = seed,
        overwrite = TRUE
      ),
      est_evidence_mcmc = beautier::create_nested_sampling_mcmc(
        epsilon = 100.0
      )
    )
    experiment_jc69_yule <- pirouette::create_experiment(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE,
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
        rng_seed = seed,
        overwrite = TRUE
      ),
      est_evidence_mcmc = beautier::create_nested_sampling_mcmc(
        epsilon = 100.0
      )
    )
    experiment_gtr_bd <- pirouette::create_experiment(
      model_type = "candidate",
      run_if = "best_candidate",
      do_measure_evidence = TRUE,
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
        rng_seed = seed,
        overwrite = TRUE
      ),
      est_evidence_mcmc = beautier::create_nested_sampling_mcmc(
        epsilon = 100.0
      )
    )
    experiments <- list(
      experiment_jc69_bd, # generative
      experiment_jc69_yule, # candidate
      experiment_gtr_bd # candidate
    )

    # Stub
    error_measure_params$errors_filename <- file.path(
      seed_folder, "mbd_nltt.csv"
    )

    pir_params <- pirouette::create_pir_params(
      alignment_params = alignment_params,
      twinning_params = twinning_params,
      experiments = experiments,
      error_measure_params = error_measure_params,
      evidence_filename = file.path(seed_folder, "mbd_marg_lik.csv")
    )
    razzo_params <- create_razzo_params(
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

#' Create parameter files to be used for testing in
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Richel J.C. Bilderbeek
#' @export
create_test_parameters_files <- function(
  project_folder_name = getwd()
) {
  create_parameters_files(
    project_folder_name,
    experiment_type = "test"
  )
}
