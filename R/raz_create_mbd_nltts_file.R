#' Create the NLTT statistics distribution files from a posterior file.
#' Assumes for a posterior file named '1x.trees'
#'   that there is a 'true' tree file called '1x.tree'
#' @inheritParams default_params_doc
#' @return nltt statistics
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_mbd_nltts_file <- function(
  parameters_filename
) {
  # Create input filenames
  raz_check_file_exists(parameters_filename) # nolint internal function
  mbd_tree_filename <- file.path(dirname(parameters_filename), "mbd.tree")
  mbd_trees_filename <- file.path(dirname(parameters_filename), "mbd.trees")
  raz_check_file_exists(mbd_tree_filename)
  raz_check_file_exists(mbd_trees_filename)

  # Read input
  mbd_tree <- ape::read.tree(file = mbd_tree_filename)
  mbd_trees <- ape::read.tree(file = mbd_trees_filename)

  # Produce nLTTs
  nltts <- raz_create_nltts(tree = mbd_tree, posterior_trees = mbd_trees) # nolint internal function

  # Save nLTTs
  mbd_nltts_filename <- file.path(dirname(parameters_filename), "mbd_nltts.csv")
  utils::write.csv(x = nltts, file = mbd_nltts_filename)
  mbd_nltts_filename
}
