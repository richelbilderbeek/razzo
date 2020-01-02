#' Create the candidate experiments
#'
#' It needs to know what the generative experiment is,
#' to exclude it from the created candidate experiments.
#' @inheritParams default_params_doc
#' @param gen_experiment the generative experiment
#' @return a list of experiments
#' @author Richel J.C. Bilderbeek
#' @export
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
    cand_experiments[[i]]$inference_model$mcmc <- razzo::get_razzo_mcmc(
      model_type = "candidate",
      folder_name = folder_name
    )
    cand_experiments[[i]]$est_evidence_mcmc <-
      razzo::create_razzo_nested_sampling_mcmc(
        folder_name =  folder_name,
        model_type = "candidate",
        index = i
      )
    cand_experiments[[i]]$inference_model$mrca_prior <-
      razzo::create_razzo_mrca_prior()
    cand_experiments[[i]]$beast2_options <-
      razzo::create_razzo_beast2_options(
        model_type = "candidate",
        folder_name = folder_name,
        rng_seed = rng_seed
      )
  }
  for (experiment in cand_experiments) {
    testit::assert(experiment$inference_model$mcmc$tracelog$filename ==
        file.path(folder_name, "mbd_best.log")
    )
  }
  cand_experiments
}
