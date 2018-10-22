#' Create an MBD tree from the razzo parameters
#' and save it as a file
#' @inheritParams default_params_doc
#' @return The tree simulated under the MBD process.
#'   It will create a file with name \code{mbd.tree}
#'   in the folder relative to the chosen parameters
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_mbd_tree_file <- function(
  parameters,
  folder_name
)
{
  mbd_sim <- raz_create_mbd_tree(parameters)

  # Save the tree to a file
  parameters_folder <- razzo::raz_get_parameters_path(parameters, folder_name)
  mbd_tree_filename <- razzo::raz_create_filename_mbd_tree(parameters, folder_name)
  save(mbd_sim, file = mbd_tree_filename)
  if (!file.exists(mbd_tree_filename)) {
    stop("mbd.tree has not been created!")
  }

  # Return the tree
  return(mbd_sim)
}
