#' Creates a \code{pir_params} with the \code{razzo} setup
#' and naming scheme
#' @inheritParams default_params_doc
#' @param has_candidates if there are candidate experiments yes/no
#' @param has_twinning do use twinning yes/no
#' @param rng_seed RNG seed for alignment simulation and inference
#' @export
create_razzo_pir_params <- function(
  has_candidates = FALSE,
  has_twinning = TRUE,
  folder_name = peregrine::get_pff_tempfile(),
  rng_seed = 1,
  error_measure_params = pirouette::create_error_measure_params()
) {
  testit::assert(beautier::is_one_bool(has_candidates))
  testit::assert(beautier::is_one_bool(has_twinning))
  testit::assert(assertive::is_a_string(folder_name))
  testit::assert(beautier::is_one_int(rng_seed))

  if (has_candidates == TRUE && rappdirs::app_dir()$os == "win") {
    stop("Cannot do model comparison on Windows")
  }
  # Alignment
  alignment_params <- razzo::create_razzo_alignment_params(
    folder_name = folder_name,
    rng_seed = rng_seed
  )
  # Experiments
  experiments <- razzo::create_razzo_experiments(
    has_candidates = has_candidates,
    folder_name = folder_name,
    rng_seed = rng_seed
  )
  # Twinning
  twinning_params <- NA
  if (isTRUE(has_twinning)) {
    twinning_params <- razzo::create_razzo_twinning_params(
      folder_name = folder_name,
      rng_seed_twin_tree = rng_seed,
      rng_seed_twin_alignment = rng_seed
    )
  }
  # Combine
  pirouette::create_pir_params(
    alignment_params = alignment_params,
    experiments = experiments,
    twinning_params = twinning_params,
    evidence_filename = razzo::get_evidence_filename(
      folder_name = folder_name,
      tree_type = "true"
    ),
    error_measure_params = error_measure_params
  )
}
