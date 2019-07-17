#' Create the parameters for one experiment.
#' Run one point of the experiment
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @aliases create_params_razzo create_razzo_params
#' @export create_params_razzo create_razzo_params
create_params_razzo <- create_razzo_params <- function(
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
  pir_params = peregrine::create_test_pff_pir_params(
    twinning_params = peregrine::create_pff_twinning_params()
  ),
  misc_params = create_misc_params()
) {
  create_razzo_params(
    mbd_params = mbd_params,
    pir_params = pir_params,
    misc_params = misc_params
  )
}
