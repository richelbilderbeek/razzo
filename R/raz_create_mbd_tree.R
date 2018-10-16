#' Create an MBD tree from the razzo parameters
#' and save it as a file
#' @inheritParams default_params_doc
#' @return nothing. Will create a file with name in \code{tree_filename}
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_mbd_tree <- function(parameters, mbd_tree_filename, folder_name)
{
  # # Create the MBD tree
  MBD_pars <- c(0.2, 0.15, 2, 0.10)
  MBD_pars <- c(parameters$lambda, parameters$mu, parameters$nu, parameters$q)
  soc <- parameters$soc
  age <- parameters$age
  cond <- parameters$cond
  seed <- parameters$seed
  set.seed(seed)
  mbd.sim  <- mbd::mbd_sim(pars = MBD_pars, soc = soc, age = age, cond = cond)
  brts <- sim$brts
  tree <- sim$tes
  l_matrix <- sim$l_matrix
  testit::assert(!is.null(brts))
  testit::assert(!is.null(tree))
  testit::assert(!is.null(l_matrix))

  # Save the tree to a file
  # parameters_folder <- raz_get_parameters_path(parameters, folder_name)
  save(mbd.sim, file = mbd_tree_filename)

  # Return the tree
  return(mbd.sim)
}
