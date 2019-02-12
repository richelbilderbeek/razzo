#' Create the parameters for one experiment.
#' Run one point of the experiment
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_razzo_params <- function(
  mbd_params,
<<<<<<< HEAD
  pir_params
) {
  razzo_params <- list(
    mbd_params = mbd_params,
    pir_params = pir_params
=======
  twinning_params = pirouette::create_twinning_params(),
  alignment_params = pirouette::create_alignment_params(
    root_sequence = "acgt", mutation_rate = 0.1
  ),
  model_select_params = list(
    pirouette::create_gen_model_select_param(alignment_params),
    pirouette::create_best_model_select_param()
  ),
  inference_params = pirouette::create_inference_params(),
  error_measure_params = pirouette::create_error_measure_params(),
  misc_params
) {
  razzo_params <- list(
    mbd_params = mbd_params,
    twinning_params = twinning_params,
    alignment_params = alignment_params,
    model_select_params = model_select_params,
    inference_params = inference_params,
    error_measure_params = error_measure_params,
    misc_params = misc_params
>>>>>>> develop
  )
  check_razzo_params(razzo_params) # nolint raket function
  razzo_params
}
