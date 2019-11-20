#' Creates a \code{pir_params} with the \code{razzo} setup
#' and naming scheme
#' @inheritParams default_params_doc
#' @param has_candidates if there are candidate experiments yes/no
#' @param rng_seed the RNG seed used in inference
#' @export
create_razzo_experiments <- function(
  has_candidates = FALSE,
  folder_name = peregrine::get_pff_tempfile(),
  rng_seed = 1
) {
  experiments <- list()
  experiments[[1]] <- razzo::create_razzo_gen_experiment(
    folder_name = folder_name,
    rng_seed = rng_seed
  )
  if (isTRUE(has_candidates)) {
    cand_experiments <- razzo::create_razzo_cand_experiments(
      experiments[[1]],
      folder_name = folder_name,
      rng_seed = rng_seed
    )
    # Copy
    for (i in seq_along(cand_experiments)) {
      experiments[[i + 1]] <- cand_experiments[[i]]
    }
  }
  for (i in seq_along(experiments)) {
    testit::assert(experiments[[i]]$beast2_options$rng_seed == rng_seed)
  }

  # Experiments
  # First is always generative
  testit::assert(experiments[[1]]$inference_conditions$model_type == "generative") # nolint indeed long
  testit::assert(experiments[[1]]$errors_filename == file.path(folder_name, "mbd_nltts_gen.csv")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$rng_seed == rng_seed)
  testit::assert(experiments[[1]]$beast2_options$input_filename == file.path(folder_name, "mbd_gen.xml")) # nolint indeed long
  testit::assert(experiments[[1]]$inference_model$mcmc$tracelog$filename == file.path(folder_name, "mbd_gen.log")) # nolint indeed long
  testit::assert(experiments[[1]]$inference_model$mcmc$treelog$filename == file.path(folder_name, "mbd_gen.trees")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$output_state_filename == file.path(folder_name, "mbd_gen.xml.state")) # nolint indeed long
  if (isTRUE(has_candidates)) {
    model_type <- "candidate"
    n_candidate_experiments <- length(experiments) - 1
    testit::assert(n_candidate_experiments >= 1)
    for (i in seq(2, 1 + n_candidate_experiments)) {
      experiments[[i]]$beast2_options$input_filename <-
        razzo::get_input_filename(folder_name = folder_name, model_type = model_type) # nolint indeed long
      experiments[[i]]$beast2_options$output_state_filename <-
        razzo::get_output_state_filename(folder_name = folder_name, model_type = model_type) # nolint indeed long
      experiments[[i]]$errors_filename <-
        razzo::get_errors_filename(folder_name = folder_name, model_type = model_type) # nolint indeed long

      testit::assert(experiments[[i]]$beast2_options$input_filename == file.path(folder_name, "mbd_best.xml")) # nolint indeed long
      testit::assert(experiments[[i]]$beast2_options$output_state_filename == file.path(folder_name, "mbd_best.xml.state")) # nolint indeed long
      testit::assert(experiments[[i]]$errors_filename == file.path(folder_name, "mbd_nltts_best.csv")) # nolint indeed long
    }
  }
  # MCMC
  for (i in seq_along(experiments)) {
    experiments[[i]]$inference_model$mcmc <- get_razzo_mcmc(
      model_type = experiments[[i]]$inference_condition$model_type,
      folder_name = folder_name
    )
    testit::assert(experiments[[i]]$inference_model$mcmc$store_every ==
      razzo::get_razzo_mcmc_store_every()
    )
    testit::assert(experiments[[i]]$inference_model$mcmc$chain_length ==
      razzo::get_razzo_mcmc_chain_length()
    )
  }
  # MRCA
  for (i in seq_along(experiments)) {
    testit::assert(
      experiments[[i]]$inference_model$mrca_prior$mrca_distr$mean$value ==
      razzo::get_razzo_crown_age()
    )
  }
  experiments
}
