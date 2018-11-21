#' Create the NLTT statistics distribution files from a posterior file.
#' Assumes for a posterior file named '1x.trees'
#'   that there is a 'true' tree file called '1x.tree'
#' @inheritParams default_params_doc
#' @return nltt statistics
#' @author Richel J.C. Bilderbeek
#' @export
create_bd_nltts_file <- function(
  parameters_filename
) {
  # Create input filenames
  check_file_exists(parameters_filename) # nolint internal function
  bd_tree_filename <- file.path(dirname(parameters_filename), "bd.tree")
  bd_trees_filename <- file.path(dirname(parameters_filename), "bd.trees")
  check_file_exists(bd_tree_filename) # nolint internal function
  check_file_exists(bd_trees_filename) # nolint internal function

  # Read input
  bd_tree <- ape::read.tree(file = bd_tree_filename)
  bd_trees <- tracerer::parse_beast_trees(filename = bd_trees_filename)

  # Produce nLTTs
  nltts <- create_nltts(tree = bd_tree, posterior_trees = bd_trees) # nolint internal function

  # Save nLTTs
  bd_nltts_filename <- file.path(dirname(parameters_filename), "bd_nltts.csv")
  utils::write.csv(x = nltts, file = bd_nltts_filename)
  bd_nltts_filename
}
