#' Create the parameters for one experiment.
#' Run one point of the experiment
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_razzo_params <- function(
  mbd_params,
  twinning_params = pirouette::create_twinning_params(),
  alignment_params,
  gen_model_select_params,
  best_model_select_params,
  inference_params,
  sampling_method,
  misc_params
) {
  razzo_params <- list(
    mbd_params = mbd_params,
    twinning_params = twinning_params,
    alignment_params = alignment_params,
    inference_params = inference_params,
    gen_model_select_params = gen_model_select_params,
    best_model_select_params = best_model_select_params,
    sampling_method = sampling_method,
    misc_params = misc_params
  )
  check_razzo_params(razzo_params) # nolint razzo function
  razzo_params
}
