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
raz_create_mbd_alignment_file <- function(
  parameters_filename
) {
  # Load input
  raz_check_file_exists(parameters_filename) # nolint internal function
  mbd_tree_filename <- file.path(dirname(parameters_filename), "mbd.tree")
  raz_check_file_exists(mbd_tree_filename)
  mbd_alignment_filename <- file.path(dirname(parameters_filename), "mbd.fasta")

  parameters <- raz_open_parameters_file(parameters_filename) # nolint internal function
  mbd_tree <- ape::read.tree(file = mbd_tree_filename)

  # Create the alignment
  alignment <- raz_create_mbd_alignment(parameters, mbd_tree) # nolint internal function

  # Save the alignment
  ape::write.FASTA(
    alignment,
    file = mbd_alignment_filename
  )
  mbd_alignment_filename
}
