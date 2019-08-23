#' Create a generative experiment that follows the razzo
#' naming conventions and article
#' @author Richel J.C. Bilderbeek
#' @export
create_razzo_gen_experiment <- function() {
  pirouette::create_gen_experiment(
    inference_conditions = pirouette::create_inference_conditions(
      model_type = "generative",
      run_if = "always",
      do_measure_evidence = TRUE
    ),
    inference_model = beautier::create_inference_model(
      tree_prior = beautier::create_bd_tree_prior(),
      mcmc = beautier::create_mcmc(
        chain_length = 1e6,
        store_every = 1e3
      ),
      mrca_prior = create_razzo_mrca_prior()
    ),
    est_evidence_mcmc = create_razzo_nested_sampling_mcmc()
  )
}
