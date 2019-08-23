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
      chain_length = 3e3, store_every = 1e3
    )
  }

  if (isTRUE(has_candidates)) {
    experiments <- experiments[1:3]
    # Different site models
    experiments[[2]]$inference_model$site_model <- beautier::create_hky_site_model() # nolint indeed long
    experiments[[3]]$inference_model$site_model <- beautier::create_tn93_site_model() # nolint indeed long
    experiments[[2]]$inference_model$clock_model <- beautier::create_strict_clock_model() # nolint indeed long
    experiments[[3]]$inference_model$clock_model <- beautier::create_strict_clock_model() # nolint indeed long
    experiments[[2]]$inference_model$tree_prior <- beautier::create_yule_tree_prior() # nolint indeed long
    experiments[[3]]$inference_model$tree_prior <- beautier::create_yule_tree_prior() # nolint indeed long
  }
  for (i in seq_along(experiments)) {
    testit::assert(
      !beautier::is_one_na(experiments[[i]]$inference_model$mrca_prior)
    )
    #experiments[[i]]$inference_model$mrca_prior <- create_razzo_mrca_prior()
  }
  testit::assert(experiments[[1]]$inference_conditions$model_type == "generative") # nolint indeed long
  # experiments[[1]]$beast2_options <- create_razzo_beast2_options(
  #   model_type = "generative",
  #   folder_name = folder_name,
  #   rng_seed = rng_seed
  # )
  testit::assert(experiments[[1]]$beast2_options$input_filename == file.path(folder_name, "mbd_gen.xml")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$output_log_filename == file.path(folder_name, "mbd_gen.log")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$output_trees_filenames == file.path(folder_name, "mbd_gen.trees")) # nolint indeed long
  testit::assert(experiments[[1]]$beast2_options$output_state_filename == file.path(folder_name, "mbd_gen.xml.state")) # nolint indeed long
  testit::assert(experiments[[1]]$errors_filename == file.path(folder_name, "mbd_nltts_gen.csv")) # nolint indeed long
  #experiments[[1]]$errors_filename <- file.path(folder_name, "mbd_nltts_gen.csv") # nolint indeed long
  if (isTRUE(has_candidates)) {
    testit::assert(length(experiments) == 3)
    for (i in seq(2, 3)) {
      # experiments[[i]]$beast2_options <- create_razzo_beast2_options(
      #   model_type = "candidate",
      #   folder_name = folder_name,
      #   rng_seed = rng_seed
      # )
      testit::assert(experiments[[i]]$beast2_options$input_filename == file.path(folder_name, "mbd_best.xml")) # nolint indeed long
      testit::assert(experiments[[i]]$beast2_options$output_log_filename == file.path(folder_name, "mbd_best.log")) # nolint indeed long
      testit::assert(experiments[[i]]$beast2_options$output_trees_filenames == file.path(folder_name, "mbd_best.trees")) # nolint indeed long
      testit::assert(experiments[[i]]$beast2_options$output_state_filename == file.path(folder_name, "mbd_best.xml.state")) # nolint indeed long
      testit::assert(experiments[[i]]$errors_filename == file.path(folder_name, "mbd_nltts_best.csv")) # nolint indeed long
      # experiments[[i]]$errors_filename <- file.path(folder_name, "mbd_nltts_best.csv") # nolint indeed long
    }
  }
  for (i in seq_along(experiments)) {
    testit::assert(
      peregrine::is_pff(experiments[[i]]$beast2_options$beast2_working_dir)
    )
    # experiments[[i]]$beast2_options$beast2_working_dir <- peregrine::get_pff_tempfile()  # nolint indeed long
  }
  experiments
}
