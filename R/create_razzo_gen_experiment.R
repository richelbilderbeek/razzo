#' Create a generative experiment that follows the razzo
#' naming conventions and article
#' @author Richel J.C. Bilderbeek
#' @inheritParams default_params_doc
#' @param rng_seed RNG seed used in inference
#' @export
create_razzo_gen_experiment <- function(
  folder_name = peregrine::get_pff_tempfile(),
  rng_seed = 1
) {
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
    beast2_options = create_razzo_beast2_options(
      model_type = "generative",
      folder_name = folder_name,
      rng_seed = rng_seed
    ),
    est_evidence_mcmc = create_razzo_nested_sampling_mcmc(),
    errors_filename = file.path(folder_name, "mbd_nltts_gen.csv")
  )
}
