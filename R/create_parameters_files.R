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
    n_replicates <- 2
    mbd_params_interval <- create_mbd_params_interval(
      lambda = 0.2,
      mu = 0.15,
      nu = 1.0,
      q = 0.1,
      seed = seq(from = 1, to = n_replicates, by = 1),
      crown_age = 15.0,
      cond = 1
    )
    parameters_filenames <- create_full_parameters_files(
      project_folder_name = project_folder_name,
      n_replicates = n_replicates,
      mbd_params_interval = mbd_params_interval
    )
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
    parameters_filenames <- create_full_parameters_files(
      project_folder_name = project_folder_name,
      n_replicates = n_replicates,
      mbd_params_interval = mbd_params_interval
    )
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
  n_replicates = 10,
  mbd_params_interval = create_mbd_params_interval(
    lambda = 0.2,
    mu = 0.15,
    nu = c(1.0, 1.5, 2.0),
    q = c(0.1, 0.15, 0.2),
    seed = seq(from = 1, to = n_replicates, by = 1),
    crown_age = 15.0,
    cond = 1
  ),
  twinning_params = pirouette::create_twinning_params(),
  alignment_params = pirouette::create_alignment_params(
    root_sequence = "aaaaccccggggttt",
    mutation_rate = 0.5 / unique(mbd_params_interval$crown_age)
  ),
  inference_params = pirouette::create_inference_params(
    mcmc = beautier::create_mcmc(chain_length = 2000, store_every = 1000),
    mrca_prior = beautier::create_mrca_prior(
      is_monophyletic = TRUE,
      mrca_distr = beautier::create_normal_distr(mean = unique(mbd_params_interval$crown_age), sigma = 0.01)
    ),
    rng_seed = 314159265 # secret trick to activate new interface
  ),
  error_measure_params = pirouette::create_error_measure_params()
) {
  # Must start at one, as the BEAST2 RNG seed must be at least one.
  index <- 1

  model_select_param <- pirouette::create_gen_model_select_param(
    alignment_params = alignment_params,
    tree_prior = beautier::create_bd_tree_prior()
  )

  # Create data folder
  data_folder_name <- "data"
  dir.create(
    file.path(project_folder_name, data_folder_name),
    showWarnings = FALSE
  )
  testit::assert(dir.exists(file.path(project_folder_name, data_folder_name)))

  # Go through all biological parameters
  n_rows <- nrow(mbd_params_interval)
  parameters_filenames <- rep(NA, n_rows * n_replicates)
  for (row_index in seq(1:n_rows)) {
    for (replicate in seq(1, n_replicates)) {
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
      seed_folder <- file.path(
        project_folder_name,
        data_folder_name,
        parsettings_name,
        mbd_params$seed
      )
      dir.create(file.path(seed_folder), showWarnings = FALSE)
      alignment_params$fasta_filename <- file.path(
        seed_folder, "mbd.fasta"
      )
      twinning_params$twin_tree_filename <- file.path(seed_folder, "bd.tree")
      twinning_params$twin_alignment_filename <- file.path(
        seed_folder, "bd.fasta"
      )
      misc_params <- list()
      misc_params$tree_filename <- "mbd.tree"
      razzo_params <- create_razzo_params(
        mbd_params = mbd_params,
        twinning_params = twinning_params,
        alignment_params = alignment_params,
        model_select_params = list(model_select_param),
        inference_params = inference_params,
        error_measure_params = error_measure_params,
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
  }
  parameters_filenames
}
