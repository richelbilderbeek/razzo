#' Create an MBD tree from the razzo parameters
#' @inheritParams default_params_doc
#' @return a list with elements that resulted from a
#' simulation under the MBD process:
#' \itemize{
#'   \item mbd_tree the reconstructed tree of class \code{phylo}
#'   \item l_matrix the L matrix
#' }
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
  mbd_sim <- mbd::mbd_sim(pars = mbd_pars, soc = soc, age = age, cond = cond)
  list(mbd_tree = mbd_sim$tes, mbd_l_matrix = mbd_sim$l_matrix)
}
