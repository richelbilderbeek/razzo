#' Create an MBD tree from the razzo parameters
#' and save it as two files: \code{mbd.tree} and \code{mbd_l_matrix.csv}
#' @inheritParams default_params_doc
#' @return The full path to the file named \code{mbd.tree}.
#'   This file contains a phylogeny simulated under the MBD process.
#'   The file will be saved in the same folder as the parameter file.
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_mbd_tree_files <- function(
  parameters_filename
) {
  parameters <- raz_open_parameters_file(parameters_filename) # nolint internal function
  mbd_sim <- raz_create_mbd_tree(parameters) # nolint internal function

  # Tree
  mbd_tree_filename <- file.path(dirname(parameters_filename), "mbd.tree")
  ape::write.tree(phy = mbd_sim$mbd_tree, file = mbd_tree_filename)

  # L matrix
  mbd_l_matrix_filename <- file.path(
    dirname(parameters_filename), "mbd_l_matrix.csv")
  utils::write.csv(x = mbd_sim$mbd_l_matrix, file = mbd_l_matrix_filename)

  list(
    mbd_tree_filename = mbd_tree_filename,
    mbd_l_matrix_filename = mbd_l_matrix_filename
  )
}
