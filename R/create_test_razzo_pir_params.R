#' Creates a \code{pir_params} with the \code{razzo} setup
#' and naming scheme
#' @inheritParams default_params_doc
#' @param has_candidates if there are candidate experiments yes/no
#' @param has_twinning do use twinning yes/no
#' @export
create_test_razzo_pir_params <- function(
  has_candidates = FALSE,
  has_twinning = TRUE,
  folder_name = peregrine::get_pff_tempfile()
) {
  ##############################################################################
  # Set up logic
  ##############################################################################
  # Alignment
  alignment_params <- pirouette::create_test_alignment_params()
  # Twinning
  twinning_params <- NA
  if (isTRUE(has_twinning)) {
    twinning_params <- pirouette::create_twinning_params()
  }
  # Experiments
  experiments <- list()
  experiments[[1]] <- pirouette::create_test_gen_experiment()
  if (isTRUE(has_candidates)) {
    experiments[[2]] <- pirouette::create_test_cand_experiment()
    # Copy BEAST2 options
    experiments[[3]] <- experiments[[2]]
    # Different site models
    experiments[[2]]$inference_model$site_model <- beautier::create_hky_site_model() # nolint indeed long
    experiments[[3]]$inference_model$site_model <- beautier::create_tn93_site_model() # nolint indeed long
  }
  for (i in seq_along(experiments)) {
    experiments[[i]]$inference_model$mrca_prior <- beautier::create_mrca_prior(
      is_monophyletic = TRUE,
      mrca_distr = beautier::create_normal_distr(
        mean = create_test_mbd_params()$crown_age,
        sigma = 0.0001
      )
    )
  }
  ##############################################################################
  # Set up filenames
  ##############################################################################
  # Alignment
  alignment_params$fasta_filename <- file.path(folder_name, "mbd.tree")
  # Twinning
  if (isTRUE(has_twinning)) {
    testit::assert(!is.na(twinning_params))
    twinning_params$twin_tree_filename <- file.path(folder_name, "mbd_twin.tree") # nolint indeed long
    twinning_params$twin_alignment_filename <- file.path(folder_name, "mbd_twin.fasta") # nolint indeed long
    twinning_params$twin_evidence_filename <- file.path(folder_name, "mbd_marg_lik_twin.csv") # nolint indeed long
  }
  # Experiments
  # First is always generative
  testit::assert(experiments[[1]]$inference_conditions$model_type == "generative") # nolint indeed long
  experiments[[1]]$beast2_options$input_filename <- file.path(folder_name, "mbd_gen.xml") # nolint indeed long
  experiments[[1]]$beast2_options$output_log_filename <- file.path(folder_name, "mbd_gen.xml") # nolint indeed long
  experiments[[1]]$beast2_options$output_trees_filenames <- file.path(folder_name, "mbd_gen.trees") # nolint indeed long
  experiments[[1]]$beast2_options$output_state_filename <- file.path(folder_name, "mbd_gen.xml.state") # nolint indeed long
  experiments[[1]]$errors_filename <- file.path(folder_name, "mbd_nltts_gen.csv") # nolint indeed long
  if (isTRUE(has_candidates)) {
    testit::assert(length(experiments) == 3)
    for (i in seq(2, 3)) {
      experiments[[i]]$beast2_options$input_filename <- file.path(folder_name, "mbd_best.xml") # nolint indeed long
      experiments[[i]]$beast2_options$output_log_filename <- file.path(folder_name, "mbd_best.xml") # nolint indeed long
      experiments[[i]]$beast2_options$output_trees_filenames <- file.path(folder_name, "mbd_best.trees") # nolint indeed long
      experiments[[i]]$beast2_options$output_state_filename <- file.path(folder_name, "mbd_best.xml.state") # nolint indeed long
      experiments[[i]]$errors_filename <- file.path(folder_name, "mbd_nltts_best.csv") # nolint indeed long
    }
  }
  for (i in seq_along(experiments)) {
    experiments[[i]]$beast2_options$beast2_working_dir <- peregrine::get_pff_tempfile()  # nolint indeed long
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
