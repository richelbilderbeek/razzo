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
  rng_seed = 1
) {
  if (has_candidates == TRUE && rappdirs::app_dir()$os == "win") {
    stop("Cannot do model comparison on Windows")
  }

  ##############################################################################
  # Set up logic
  ##############################################################################
  # Alignment
  alignment_params <- create_razzo_alignment_params(
    folder_name = folder_name,
    rng_seed = rng_seed
  )
  # Experiments
  experiments <- create_razzo_experiments(
    has_candidates = has_candidates,
    folder_name = folder_name,
    rng_seed = rng_seed
  )
  # Twinning
  twinning_params <- NA
  if (isTRUE(has_twinning)) {
    twinning_params <- create_razzo_twinning_params(folder_name = folder_name)
  }
  ##############################################################################
  # Set up filenames
  ##############################################################################
  # Alignment
  testit::assert(
    alignment_params$fasta_filename == file.path(folder_name, "mbd.fasta")
  )
  # Twinning
  if (isTRUE(has_twinning)) {
    testit::assert(!is.na(twinning_params))
    testit::assert(twinning_params$twin_tree_filename == file.path(folder_name, "mbd_twin.tree")) # nolint indeed long
    testit::assert(twinning_params$twin_alignment_filename == file.path(folder_name, "mbd_twin.fasta")) # nolint indeed long
    testit::assert(twinning_params$twin_evidence_filename == file.path(folder_name, "mbd_marg_lik_twin.csv")) # nolint indeed long
  }
  # Experiments
  # First is always generative
  testit::assert(experiments[[1]]$inference_conditions$model_type == "generative") # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$input_filename == file.path(folder_name, "mbd_gen.xml")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$output_log_filename == file.path(folder_name, "mbd_gen.log")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$output_trees_filenames == file.path(folder_name, "mbd_gen.trees")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$output_state_filename == file.path(folder_name, "mbd_gen.xml.state")) # nolint indeed long
  testit::assert(experiments[[1]]$errors_filename == file.path(folder_name, "mbd_nltts_gen.csv")) # nolint indeed long
  if (isTRUE(has_candidates)) {
    n_candidate_experiments <- length(experiments) - 1
    testit::assert(n_candidate_experiments >= 1)
    for (i in seq(2, 1 + n_candidate_experiments)) {
      testit::assert(experiments[[i]]$beast2_options$input_filename == file.path(folder_name, "mbd_best.xml")) # nolint indeed long
      testit::assert(experiments[[i]]$beast2_options$output_log_filename == file.path(folder_name, "mbd_best.log")) # nolint indeed long
      testit::assert(experiments[[i]]$beast2_options$output_trees_filenames == file.path(folder_name, "mbd_best.trees")) # nolint indeed long
      testit::assert(experiments[[i]]$beast2_options$output_state_filename == file.path(folder_name, "mbd_best.xml.state")) # nolint indeed long
      testit::assert(experiments[[i]]$errors_filename == file.path(folder_name, "mbd_nltts_best.csv")) # nolint indeed long
    }
  }
  for (i in seq_along(experiments)) {
    testit::assert(
      peregrine::is_pff(experiments[[i]]$beast2_options$beast2_working_dir)
    )
  }
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
