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
    twinning_params <- razzo::create_razzo_twinning_params(folder_name)
  }
  # Experiments
  experiments <- razzo::create_test_razzo_experiments(
    has_candidates = has_candidates,
    folder_name = folder_name,
    rng_seed = rng_seed
  )
  ##############################################################################
  # Set up filenames
  ##############################################################################
  # Alignment
  alignment_params$fasta_filename <- razzo::get_alignment_filename(
    folder_name = folder_name,
    tree_type = "true"
  )
  # Experiments
  # Done by 'create_test_razzo_experiments'
  # Evidence filename
  evidence_filename <-
    razzo::get_evidence_filename(folder_name = folder_name, tree_type = "true")
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
