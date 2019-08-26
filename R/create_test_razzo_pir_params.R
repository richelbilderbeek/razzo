#' Creates a \code{pir_params} with the \code{razzo} setup
#' and naming scheme
#' @inheritParams default_params_doc
#' @param has_candidates if there are candidate experiments yes/no
#' @param has_twinning do use twinning yes/no
#' @param rng_seed RNG seed for alignment simulation and inference
#' @export
create_test_razzo_pir_params <- function(
  has_candidates = FALSE,
  has_twinning = TRUE,
  folder_name = peregrine::get_pff_tempfile(),
  rng_seed = 1
) {
  ##############################################################################
  # Set up logic
  ##############################################################################
  # Alignment
  alignment_params <- pirouette::create_test_alignment_params(
    rng_seed = rng_seed
  )
  # Twinning
  twinning_params <- NA
  if (isTRUE(has_twinning)) {
    twinning_params <- create_razzo_twinning_params(folder_name)
  }
  # Experiments
  experiments <- create_test_razzo_experiments(
    has_candidates = has_candidates,
    folder_name = folder_name,
    rng_seed = rng_seed
  )
  # experiments <- list()
  # experiments[[1]] <- pirouette::create_test_gen_experiment()
  # if (isTRUE(has_candidates)) {
  #   experiments[[2]] <- pirouette::create_test_cand_experiment()
  #   # Copy BEAST2 options
  #   experiments[[3]] <- experiments[[2]]
  #   # Different site models
  #   experiments[[2]]$inference_model$site_model <- beautier::create_hky_site_model() # nolint indeed long
  #   experiments[[3]]$inference_model$site_model <- beautier::create_tn93_site_model() # nolint indeed long
  # }
  # for (i in seq_along(experiments)) {
  #   experiments[[i]]$inference_model$mrca_prior <- create_razzo_mrca_prior()
  # }
  ##############################################################################
  # Set up filenames
  ##############################################################################
  # Alignment
  alignment_params$fasta_filename <- file.path(folder_name, "mbd.fasta")
  # Experiments
  # Done by 'create_test_razzo_experiments'
  # Evidence filename
  evidence_filename <- file.path(folder_name, "mbd_marg_lik.csv")
  ##############################################################################
  # Combine
  ##############################################################################
  pirouette::create_pir_params(
    alignment_params = alignment_params,
    experiments = experiments,
    twinning_params = twinning_params,
    evidence_filename = evidence_filename
  )
}
