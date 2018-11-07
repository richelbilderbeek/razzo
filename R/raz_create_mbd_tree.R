#' Create an MBD tree from the razzo parameters
#' @inheritParams default_params_doc
#' @return a list with elements that resulted from a
#' simulation under the MBD process:
#' \itemize{
#'   \item mbd_tree the reconstructed tree of class \code{phylo}
#'   \item l_matrix the L matrix
#' }
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
raz_create_mbd_tree <- function(
  parameters
) {
  mbd_pars <- c(parameters$lambda, parameters$mu, parameters$nu, parameters$q)
  n_0 <- 2 # two species at time zero
  age <- parameters$crown_age
  cond <- 1 # Condition on non-extinction
  seed <- parameters$seed
  testit::assert(!is.null(seed))
  testit::assert(!is.null(age))
  testit::assert(!is.null(n_0))
  testit::assert(!is.null(cond))

  set.seed(seed)
  # Use 'soc' argument until after mbd overhaul
  mbd_sim <- mbd::mbd_sim(pars = mbd_pars, n_0 = n_0, age = age, cond = cond)

  # Use 'tes' argument until after mbd overhaul
  list(mbd_tree = mbd_sim$reconstructed_tree, mbd_l_matrix = mbd_sim$l_matrix)
}
