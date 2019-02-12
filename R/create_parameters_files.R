#' Create all parameter files  in
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @param experiment_type the type of experiment,
#'   can be either \code{test} or \code{full}
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
    create_test_parameters_files(project_folder_name = project_folder_name) # nolint internal function
  } else {
    create_full_parameters_files(project_folder_name = project_folder_name) # nolint internal function
  }
}

#' Create the parameter files as used in the experiment
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_full_parameters_files <- function(
  project_folder_name = getwd()
) {
  stop("Won't work, see 'create_test_parameters_files'")
  # Just use the parameter combinations in the article
  lambda_interval <- c(0.2, 0.2)
  mu_interval <- c(0.15, 0.15)
  nu_interval <- c(1.0, 1.0) # Testing
  q_interval <- c(0.10, 0.10) # Testing
  seed_interval <- 1:2
  crown_age <- 15
  sequence_length <- 100 # Testing
  sample_interval <- 1000
  chain_length <- 1e+6 # parameter L_c
  sub_chain_length <- 1000 # parameter L_sc
  clock_model_interval <- get_clock_models() # nolint internal function
  site_model_interval <- get_site_models() # nolint internal function

  lambda_interval <- unique(lambda_interval)
  mu_interval     <- unique(mu_interval)
  nu_interval     <- unique(nu_interval)
  q_interval      <- unique(q_interval)

  l_pars <- length(lambda_interval) *
    length(mu_interval) *
    length(nu_interval) *
    length(q_interval)

  data_folder_name <- "data"

  # Do not warn if the folder already exists
  dir.create(
    file.path(project_folder_name, data_folder_name),
    showWarnings = FALSE
  )
  testit::assert(dir.exists(file.path(project_folder_name, data_folder_name)))
  parameters_filenames <- rep(NA, l_pars)
  i <- 1
  for (lambda in lambda_interval) {
    for (mu in mu_interval) {
      for (nu in nu_interval) {
        for (q in q_interval) {
          parsettings_name <- paste0(lambda, "-", mu, "-", nu, "-", q)
          dir.create(file.path(project_folder_name,
                               data_folder_name,
                               parsettings_name),
                     showWarnings = FALSE)
          for (seed in seed_interval) {
            seed_folder <- file.path(
              project_folder_name,
              data_folder_name,
              parsettings_name,
              seed
            )
            dir.create(file.path(seed_folder),
                       showWarnings = FALSE)
            for (clock_model in clock_model_interval) {
              for (site_model in site_model_interval) {
                model_folder <- file.path(
                  seed_folder,
                  paste0(clock_model, "-", site_model)
                )
                dir.create(file.path(model_folder),
                           showWarnings = FALSE)

                parameters <- create_params(
                  lambda = lambda,
                  mu = mu,
                  nu = nu,
                  q = q,
                  seed = seed,
                  crown_age = crown_age,
                  sequence_length = sequence_length,
                  sample_interval = sample_interval,
                  chain_length = chain_length,
                  sub_chain_length = sub_chain_length,
                  clock_model = clock_model,
                  site_model = site_model
                )

                parameters_filenames[i] <- file.path(
                  model_folder,
                  "parameters.csv"
                )
                utils::write.csv(parameters, file = parameters_filenames[i])
                i <- i + 1
              }
            }
          }
        }
      }
    }
  }
  parameters_filenames
}

#' Create all testing parameter files in
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_test_parameters_files <- function(
  project_folder_name = getwd(),
  n_replicates = 2,
  root_sequence = "acgt",
  verbose = FALSE
) {
  # Must start at one, as the BEAST2 RNG seed must be at least one.
  index <- 1
  bio_params <- expand.grid(
    lambda = 0.2,
    mu = 0.15,
    nu = 1.0,
    q = 0.1
  )

  # Create data folder
  data_folder_name <- "data"
  dir.create(
    file.path(project_folder_name, data_folder_name),
    showWarnings = FALSE
  )
  testit::assert(dir.exists(file.path(project_folder_name, data_folder_name)))

  # Go through all biological parameters
  n_rows <- nrow(bio_params)
  parameters_filenames <- rep(NA, n_rows * n_replicates)
  for (row_index in seq(1:n_rows)) {
    # Extract the biological parameters and create a folder for them
    lambda <- bio_params$lambda
    mu <- bio_params$mu
    nu <- bio_params$nu
    q <- bio_params$q
    mbd_params <- becosys::create_mbd_params(
      lambda = lambda,
      mu = mu,
      nu = nu,
      q = q
    )
    parsettings_name <- paste0(lambda, "-", mu, "-", nu, "-", q)
    dir.create(
      file.path(project_folder_name,
      data_folder_name,
      parsettings_name),
      showWarnings = FALSE
    )
    for (replicate in seq(1, n_replicates)) {
      seed <- index
      seed_folder <- file.path(
        project_folder_name,
        data_folder_name,
        parsettings_name,
        seed
      )
      dir.create(file.path(seed_folder), showWarnings = FALSE)
      alignment_params <- create_alignment_params(
        root_sequence = root_sequence,
        mutation_rate = pirouette::create_standard_mutation_rate,
        site_model = beautier::create_jc69_site_model(),
        clock_model = beautier::create_strict_clock_model(),
        rng_seed = seed,
        fasta_filename = file.path(seed_folder, "mbd.fasta")
      )
      twinning_params <- create_twinning_params(
        rng_seed = seed,
        twin_model = "bd",
        method = "max_clade_cred",
        n_replicas = n_replicates,
        twin_tree_filename = file.path(seed_folder, "bd.tree"),
        twin_alignment_filename = file.path(seed_folder, "bd.fasta"),
        twin_evidence_filename = file.path(seed_folder, "mbd_marg_lik.csv")
      )

      mcmc <- create_mcmc(chain_length = 3000, store_every = 1000)
      mrca_prior <- create_mrca_prior(
        is_monophyletic = TRUE,
        mrca_distr = create_normal_distr(mean = 15.0, sigma = 0.0001)
      )

      # name                |model_type | run_if         | measure  | inference  # nolint this is no commented code
      #                     |           |                | evidence | model
      # --------------------|-----------|----------------|----------|-----------
      # experiment_jc69_bd  |generative | always         |TRUE      |JC69, BD   # nolint this is no commented code
      # experiment_jc69_yule|candidate  | best_candidate |TRUE      |JC69, Yule # nolint this is no commented code
      # experiment_gtr_bd   |candidate  | best_candidate |TRUE      |GTR, BD    # nolint this is no commented code
      #
      # Sure, a fourth model (gtr_yule) would finish the pattern,
      # but this woul also needlessly slow down our tests
      experiment_jc69_bd <- create_experiment(
        model_type = "generative",
        run_if = "always",
        do_measure_evidence = TRUE,
        inference_model = create_inference_model(
          site_model = create_jc69_site_model(),
          tree_prior = create_bd_tree_prior(),
          mcmc = mcmc,
          mrca_prior = mrca_prior
        ),
        beast2_options = create_beast2_options(
          input_filename = file.path(seed_folder, "mbd_gen.xml"),
          output_log_filename = file.path(seed_folder, "mbd_gen.log"),
          output_trees_filenames = file.path(seed_folder, "mbd_gen.trees"),
          output_state_filename = file.path(seed_folder, "mbd_gen.xml.state"),
          rng_seed = seed,
          overwrite = TRUE
        ),
        est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
      )
      experiment_jc69_yule <- create_experiment(
        model_type = "candidate",
        run_if = "best_candidate",
        do_measure_evidence = TRUE,
        inference_model = create_inference_model(
          site_model = create_jc69_site_model(),
          tree_prior = create_yule_tree_prior(),
          mcmc = mcmc,
          mrca_prior = mrca_prior
        ),
        beast2_options = create_beast2_options(
          input_filename = file.path(seed_folder, "mbd_best.xml"),
          output_log_filename = file.path(seed_folder, "mbd_best.log"),
          output_trees_filenames = file.path(seed_folder, "mbd_best.trees"),
          output_state_filename = file.path(seed_folder, "mbd_best.xml.state"),
          rng_seed = seed,
          overwrite = TRUE
        ),
        est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
      )
      experiment_gtr_bd <- create_experiment(
        model_type = "candidate",
        run_if = "best_candidate",
        do_measure_evidence = TRUE,
        inference_model = create_inference_model(
          site_model = create_gtr_site_model(),
          tree_prior = create_bd_tree_prior(),
          mcmc = mcmc,
          mrca_prior = mrca_prior
        ),
        beast2_options = create_beast2_options(
          input_filename = file.path(seed_folder, "mbd_best.xml"),
          output_log_filename = file.path(seed_folder, "mbd_best.log"),
          output_trees_filenames = file.path(seed_folder, "mbd_best.trees"),
          output_state_filename = file.path(seed_folder, "mbd_best.xml.state"),
          rng_seed = seed,
          overwrite = TRUE
        ),
        est_evidence_mcmc = create_nested_sampling_mcmc(epsilon = 100.0)
      )
      experiments <- list(
        experiment_jc69_bd, # generative
        experiment_jc69_yule, # candidate
        experiment_gtr_bd # candidate
      )
      # TODO: add filename of .csv file, see
      # https://github.com/richelbilderbeek/pirouette/issues/115
      error_measure_params <- create_error_measure_params()

      pir_params <- create_pir_params(
        alignment_params = alignment_params,
        twinning_params = twinning_params,
        experiments = experiments,
        error_measure_params = error_measure_params,
        evidence_filename = file.path(seed_folder, "mbd_marg_lik.csv"),
        verbose = verbose
      )
      razzo_params <- create_razzo_params(
        mbd_params = mbd_params,
        pir_params = pir_params
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
  }
  parameters_filenames
}
