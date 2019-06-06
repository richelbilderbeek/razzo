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
  mbd_params = create_test_mbd_params(),
  pir_params = pirouette::create_test_pir_params(
    experiments = list(
      pirouette::create_test_gen_experiment(
        inference_model = beastier::create_inference_model(
          mrca_prior = beastier::create_mrca_prior(
            mrca_distr = beastier::create_normal_distr(mean = 15.0, sigma = 0.001),
            is_monophyletic = TRUE
          )
        )
      )
    ),
    twinning_params = pirouette::create_twinning_params()
  ),
  misc_params = create_misc_params()
) {
  create_razzo_params(
    mbd_params = mbd_params,
    pir_params = pir_params,
    misc_params = misc_params
  )
}
