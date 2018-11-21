#' @title Create an MBD alignment from a parameters file in the same folder
#' @description Create an MBD alignment
#'   from a parameters file in the same folder
#' as that parameters file.
#' For example, for a path '/my_folder/parameters.csv', this function creates:
#' '/my_folder/mbd.fasta'.
#' @inheritParams default_params_doc
#' @return full path to the created file
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_mbd_alignment_file <- function(
  parameters_filename
) {
  # Load input
  check_file_exists(parameters_filename) # nolint internal function
  mbd_tree_filename <- file.path(dirname(parameters_filename), "mbd.tree")
  check_file_exists(mbd_tree_filename) # nolint internal function
  mbd_alignment_filename <- file.path(dirname(parameters_filename), "mbd.fasta")
  bd_tree_filename <- file.path(dirname(parameters_filename), "bd.tree")
  check_file_exists(bd_tree_filename) # nolint internal function

  parameters <- open_parameters_file(parameters_filename) # nolint internal function
  mbd_tree <- ape::read.tree(file = mbd_tree_filename)
  bd_tree <- ape::read.tree(file = bd_tree_filename)

  # Create the alignment
  alignment <- create_mbd_alignment( # nolint internal function
    parameters = parameters,
    mbd_tree = mbd_tree,
    bd_tree = bd_tree
  ) # nolint internal function

  # Save the alignment
  ape::write.FASTA(
    alignment,
    file = mbd_alignment_filename
  )
  mbd_alignment_filename
}
