#' Create the parameters for one experiment.
#'
#' Run one point of the experiment
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_razzo_params <- function(
  mbd_params,
  pir_params,
  misc_params
) {
  razzo_params <- list(
    mbd_params = mbd_params,
    pir_params = pir_params,
    misc_params = misc_params
  )
  check_razzo_params(razzo_params) # nolint razzo function
  razzo_params
}

#' Create some testing parameters for one experiment.
#' Run one point of the experiment
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_test_razzo_params <- function(
) {
  create_razzo_params(
    mbd_params = create_test_mbd_params(),
    pir_params = pirouette::create_pir_params(
      alignment_params = pirouette::create_test_alignment_params(),
      experiments = list(
        pirouette::create_experiment(
          model_type = "generative",
          run_if = "always",
          do_measure_evidence = FALSE,
          inference_model = beautier::create_inference_model(
            mrca_prior = beautier::create_mrca_prior(
              mrca_distr = beautier::create_normal_distr(
                mean = 15.0, sigma = 0.001
              ),
              is_monophyletic = TRUE
            ),
            mcmc = beautier::create_mcmc(
              chain_length = 2000, store_every = 1000
            )
          )
        )
      ),
      twinning_params = pirouette::create_twinning_params()
    ),
    misc_params = create_misc_params()
  )
}
