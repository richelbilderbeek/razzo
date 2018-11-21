#' Create an MBD tree from the razzo parameters
#' @inheritParams default_params_doc
#' @description This works like create_mbd_tree but instead of specifying
#' the parameter nu, the user has to specify the number of nu-events in
#' the simulated process
#' @return a list with elements that resulted from a
#' simulation under the MBD process:
#' \itemize{
#'   \item mbd_tree the reconstructed tree of class \code{phylo}
#'   \item l_matrix the L matrix
#' }
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_mbd_tree_events <- function(
  parameters,
  nu_events
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
  testit::assert(nu_events >= 0)

  new_nu <- nu_events / age
  new_mbd_pars <- mbd_pars
  new_mbd_pars[3] <- new_nu

  set.seed(seed)
  number_of_events <- -1
  while (number_of_events != nu_events) {
    mbd_sim <- mbd::mbd_sim(
      pars = new_mbd_pars,
      n_0 = n_0,
      age = age,
      cond = cond
    )
    number_of_events <- length(
      unique(mbd_sim$brts[duplicated(mbd_sim$brts)])
    )
  }

  list(mbd_tree = mbd_sim$reconstructed_tree, mbd_l_matrix = mbd_sim$l_matrix)
}
