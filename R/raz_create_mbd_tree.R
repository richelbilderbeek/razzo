#' Create an MBD tree from the razzo parameters
#' @inheritParams default_params_doc
#' @return a reconstructed tree of class \code{[hylo},
#'    as simulated under the MBD process
#' @author Giovanni Laudanno and Richel J.C. Bilderbeek
#' @export
raz_create_mbd_tree <- function(
  parameters
) {
  mbd_pars <- c(parameters$lambda, parameters$mu, parameters$nu, parameters$q)
  soc <- parameters$soc
  age <- parameters$age
  cond <- parameters$cond
  seed <- parameters$seed
  set.seed(seed)
  mbd::mbd_sim(pars = mbd_pars, soc = soc, age = age, cond = cond)$tes
}
