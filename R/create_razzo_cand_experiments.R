#' Create the candidate experiments
#' @param gen_experiment the generative experiment
create_razzo_cand_experiments <- function(
  gen_experiment,
  folder_name = peregrine::get_pff_tempfile(),
  rng_seed = 1
) {
  testit::assert(gen_experiment$inference_conditions$model_type == "generative")

  cand_experiments <- NA

  # For a list of the options, see
  # https://github.com/richelbilderbeek/razzo/issues/261#issue-488720156 # nolint indeed a URL
  option <- 3

  cand_experiments <- NA

  if (option == 2) {
    cand_experiments <- pirouette::create_all_experiments(
      exclude_model = gen_experiment$inference_model
    )
  }
  if (option == 3) {
    cand_experiments <- pirouette::create_all_experiments(
      site_models = list(
        beautier::create_jc69_site_model(),
        beautier::create_hky_site_model()
      ),
      clock_models = list(beautier::create_strict_clock_model()),
      tree_priors = list(
        beautier::create_yule_tree_prior(),
        beautier::create_bd_tree_prior()
      ),
      exclude_model = gen_experiment$inference_model
    )
  }

  testit::assert(!beautier::is_one_na(cand_experiments))

  # Adapt to razzo
  for (i in seq_along(cand_experiments)) {
    cand_experiments[[i]]$inference_model$mcmc <- beautier::create_mcmc(
      store_every = 1e3,
      chain_length = 1e6
    )
    cand_experiments[[i]]$est_evidence_mcmc <- create_razzo_nested_sampling_mcmc()
    cand_experiments[[i]]$inference_model$mrca_prior <- create_razzo_mrca_prior()
    cand_experiments[[i]]$est_evidence_mcmc <- create_razzo_nested_sampling_mcmc()
    cand_experiments[[i]]$beast2_options <- create_razzo_beast2_options(
      model_type = "candidate", folder_name = folder_name, rng_seed = rng_seed
    )
  }
  cand_experiments
}