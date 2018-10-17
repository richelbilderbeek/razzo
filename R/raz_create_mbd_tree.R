#' Create an MBD tree from the razzo parameters
#' and save it as a file
#' @inheritParams default_params_doc
#' @return The tree simulated under the MBD process. It will create a file with name \code{mbd.tree} in the folder relative to the chosen parameters
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_mbd_tree <- function(
  parameters,
  folder_name
)
{
  # # Create the MBD tree
  MBD_pars <- c(parameters$lambda, parameters$mu, parameters$nu, parameters$q)
  soc <- parameters$soc
  age <- parameters$age
  cond <- parameters$cond
  seed <- parameters$seed
  set.seed(seed)
  mbd.sim  <- mbd::mbd_sim(pars = MBD_pars, soc = soc, age = age, cond = cond)
  brts <- mbd.sim$brts
  tree <- mbd.sim$tes
  l_matrix <- mbd.sim$l_matrix
  testit::assert(!is.null(brts))
  testit::assert(!is.null(tree))
  testit::assert(!is.null(l_matrix))

  # Save the tree to a file
  parameters_folder <- razzo::raz_get_parameters_path(parameters, folder_name)
  mbd_tree_filename <- razzo::raz_filename.mbd_tree(parameters, folder_name)
  save(mbd.sim, file = mbd_tree_filename)
  if (!file.exists(mbd_tree_filename)) stop('mbd.tree has not been created!')

  # Return the tree
  return(mbd.sim)
}
