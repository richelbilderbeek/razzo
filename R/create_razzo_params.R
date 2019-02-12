#' Create the parameters for one experiment.
#' Run one point of the experiment
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_razzo_params <- function(
  mbd_params,
  pir_params
) {
  razzo_params <- list(
    mbd_params = mbd_params,
    pir_params = pir_params
  )
  check_razzo_params(razzo_params) # nolint raket function
  razzo_params
}
