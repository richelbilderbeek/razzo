#' Create a set of testing experiments
#' @inheritParams default_params_doc
#' @param has_candidates if there are candidate experiments yes/no
#' @param rng_seed the RNG seed used in inference
#' @export
create_test_razzo_experiments <- function(
  has_candidates = TRUE,
  folder_name = peregrine::get_pff_tempfile(),
  rng_seed = 1
) {
  experiments <- create_razzo_experiments(
    has_candidates = has_candidates,
    folder_name = folder_name,
    rng_seed = rng_seed
  )
  # Shorten the MCMC
  for (i in seq_along(experiments)) {
    experiments[[i]]$inference_model$mcmc <- beautier::create_mcmc(
      chain_length = 3e3,
      store_every = 1e3,
      tracelog = beautier::create_tracelog(
        filename = get_tracelog_filename(
          folder_name = folder_name,
          model_type = experiments[[i]]$inference_conditions$model_type
        )
      ),
      treelog = beautier::create_treelog(
        filename = get_treelog_filename(
          folder_name = folder_name,
          model_type = experiments[[i]]$inference_conditions$model_type
        )
      )
    )
    experiments[[i]]$est_evidence_mcmc$chain_length <- 3e3
    experiments[[i]]$est_evidence_mcmc$store_every <- 1e3

  }

  if (isTRUE(has_candidates)) {
    experiments <- experiments[1:3]
    # Different site models
    experiments[[2]]$inference_model$site_model <-
      beautier::create_hky_site_model()
    experiments[[3]]$inference_model$site_model <-
      beautier::create_tn93_site_model()
    experiments[[2]]$inference_model$clock_model <-
      beautier::create_strict_clock_model()
    experiments[[3]]$inference_model$clock_model <-
      beautier::create_strict_clock_model()
    experiments[[2]]$inference_model$tree_prior <-
      beautier::create_yule_tree_prior()
    experiments[[3]]$inference_model$tree_prior <-
      beautier::create_yule_tree_prior()
  }
  # Experiments' filenames are correctly set up by 'create_razzo_experiments'
  experiments
}
