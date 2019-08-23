#' Creates a \code{pir_params} with the \code{razzo} setup
#' and naming scheme
#' @inheritParams default_params_doc
#' @param has_candidates if there are candidate experiments yes/no
#' @param has_twinning do use twinning yes/no
#' @export
create_razzo_experiments <- function(
  has_candidates = FALSE,
  folder_name = peregrine::get_pff_tempfile(),
  rng_seed = 1
) {
  experiments <- list()
  experiments[[1]] <- create_razzo_gen_experiment()
  if (isTRUE(has_candidates)) {
    cand_experiments <- pirouette::create_all_experiments(
      exclude_model = experiments[[1]]$inference_model
    )
    for (i in seq_along(cand_experiments)) {
      experiments[[i + 1]] <- cand_experiments[[i]]
    }
  }
  # MCMC
  for (i in seq_along(experiments)) {
    experiments[[i]]$inference_model$mcmc <- beautier::create_mcmc(
      store_every = 1000,
      chain_length = 1e6
    )
    experiments[[i]]$est_evidence_mcmc <- create_razzo_nested_sampling_mcmc()
  }
  # MRCA
  for (i in seq_along(experiments)) {
    experiments[[i]]$inference_model$mrca_prior <- create_razzo_mrca_prior()
  }

  # Experiments
  # First is always generative
  testit::assert(experiments[[1]]$inference_conditions$model_type == "generative") # nolint indeed long
  experiments[[1]]$beast2_options <- create_razzo_beast2_options(
    model_type = "generative",
    folder_name = folder_name,
    rng_seed = rng_seed
  )
  testit::assert(experiments[[1]]$beast2_options$input_filename == file.path(folder_name, "mbd_gen.xml")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$output_log_filename == file.path(folder_name, "mbd_gen.log")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$output_trees_filenames == file.path(folder_name, "mbd_gen.trees")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$output_state_filename == file.path(folder_name, "mbd_gen.xml.state")) # nolint indeed long
  experiments[[1]]$errors_filename <- file.path(folder_name, "mbd_nltts_gen.csv") # nolint indeed long
  if (isTRUE(has_candidates)) {
    n_candidate_experiments <- length(experiments) - 1
    testit::assert(n_candidate_experiments >= 1)
    for (i in seq(2, 1 + n_candidate_experiments)) {
      experiments[[i]]$beast2_options <- create_razzo_beast2_options(
        model_type = "candidate",
        folder_name = folder_name,
        rng_seed = rng_seed
      )
      testit::assert(experiments[[i]]$beast2_options$input_filename == file.path(folder_name, "mbd_best.xml")) # nolint indeed long
      testit::assert(experiments[[i]]$beast2_options$output_log_filename == file.path(folder_name, "mbd_best.log")) # nolint indeed long
      testit::assert(experiments[[i]]$beast2_options$output_trees_filenames == file.path(folder_name, "mbd_best.trees")) # nolint indeed long
      testit::assert(experiments[[i]]$beast2_options$output_state_filename == file.path(folder_name, "mbd_best.xml.state")) # nolint indeed long
      experiments[[i]]$errors_filename <- file.path(folder_name, "mbd_nltts_best.csv") # nolint indeed long
    }
  }
  for (i in seq_along(experiments)) {
    testit::assert(
      peregrine::is_pff(experiments[[i]]$beast2_options$beast2_working_dir)
    )
    experiments[[i]]$beast2_options$beast2_working_dir <- peregrine::get_pff_tempfile()  # nolint indeed long
  }
  experiments
}
