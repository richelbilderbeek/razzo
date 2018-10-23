#' Create an MBD tree from the razzo parameters
#' and save it as a file
#' @inheritParams default_params_doc
#' @return The full path to the file named \code{mbd.tree}.
#'   This file contains a phylogeny simulated under the MBD process.
#'   The file will be saved in the same folder as the parameter file.
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_mbd_tree_file <- function(
  parameters_filename
) {

  mbd_tree <- raz_create_mbd_tree(parameters)
  mbd_tree_filename <- file.path(dirname(parameters_filename), "mbd.tree")
  ape::write.tree(phy = mbd_tree, file = mbd_tree_filename)
  mbd_tree_filename
}
