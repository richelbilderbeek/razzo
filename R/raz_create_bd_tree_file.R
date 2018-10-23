#' Create an BD twin tree from an MBD tree
#' and save it as a file
#' @inheritParams default_params_doc
#' @return The twin BD tree obtained from the corresponding MBD tree.
#'   It will create a file with name \code{bd.tree}
#'   in the folder relative to the chosen parameters
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_bd_tree_file <- function(
  parameters_filename
) {
  testit::assert(file.exists(parameters_filename))
  mbd_tree_filename <- file.path(dirname(parameters_filename), "mbd.tree")
  testit::assert(file.exists(mbd_tree_filename))
  mbd_l_matrix_filename <- file.path(
    dirname(parameters_filename), "mbd_l_matrix.csv")
  testit::assert(file.exists(mbd_l_matrix_filename))

  parameters <- raz_open_parameters_file(parameters_filename)
  mbd_tree <- ape::read.tree(file = mbd_tree_filename)
  mbd_l_matrix <- as.matrix(utils::read.csv(file = mbd_l_matrix_filename))[, -1]

  bd_tree <- raz_create_bd_tree(
    parameters = parameters,
    mbd_tree = mbd_tree,
    mbd_l_matrix = mbd_l_matrix
  )
  bd_tree_filename <- file.path(dirname(parameters_filename), "bd.tree")
  ape::write.tree(phy = bd_tree, file = bd_tree_filename)
  bd_tree_filename
}
