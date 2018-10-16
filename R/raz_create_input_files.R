#' Create the input files from a parameters file in the same folder
#' as that parameters file.
#' For example, for a path '/my_folder/parameters.csv', this function creates:
#' \itemize{
#'   \item '/my_folder/mbd.tree': the MBD tree
#'   \item '/my_folder/mbd.fasta': the MBD alignment
#'   \item '/my_folder/bd.tree': the MBD tree
#'   \item '/my_folder/bd.fasta': the MBD alignment
#' }
#' @inheritParams default_params_doc
#' @return names of the files created
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_input_files <- function(parameters_filename) {

  # TODO: check for parameter file and use its parameters
  if (1 == 2) {
    testit::assert(file.exists(parameters_filename))
  }

  # Read the parameters
  parameters <- raz_open_parameters_file(parameters_filename)
  testit::assert(parameters$lambda >= 0.0)

  # Get the four filenames
  folder_name <- dirname(parameters_filename)
  mbd_tree_filename <- file.path(folder_name, "mbd.tree")
  mbd_alignment_filename <- file.path(folder_name, "mbd.fasta")
  bd_tree_filename <- file.path(folder_name, "bd.tree")
  bd_alignment_filename <- file.path(folder_name, "bd.fasta")

  # TODO: create the four files
  if (1 == 2) {
    # Create an MBD tree
    raz_create_mbd_tree(parameters, mbd_tree_filename)
    testit::assert(file.exists(mbd_tree_filename))
  }

  # Return the filenames
  input_filenames <- c(
    mbd_tree_filename,
    mbd_alignment_filename,
    bd_tree_filename,
    bd_alignment_filename
  )
  input_filenames
}
