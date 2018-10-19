#' Create filename: mbd_tree
#' @inheritParams default_params_doc
#' @return name of the mbd_tree file
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_filename.mbd_tree <- function(
  parameters,
  folder_name
)
{
  parameters_folder <- razzo::raz_get_parameters_path(parameters, folder_name = folder_name)
  mbd_tree_filename <- file.path(parameters_folder, "mbd.tree")
  return(mbd_tree_filename)
}

#' Create filename: bd_tree
#' @inheritParams default_params_doc
#' @return name of the bd_tree file
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_filename.bd_tree <- function(
  parameters,
  folder_name
)
{
  parameters_folder <- razzo::raz_get_parameters_path(parameters, folder_name = folder_name)
  bd_tree_filename <- file.path(parameters_folder, "bd.tree")
  return(bd_tree_filename)
}

#' Create filename: mbd_alignment
#' @inheritParams default_params_doc
#' @return name of the mbd_alignment file
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_filename.mbd_alignment <- function(
  parameters,
  folder_name
)
{
  parameters_folder <- razzo::raz_get_parameters_path(parameters, folder_name = folder_name)
  mbd_fasta_filename <- file.path(parameters_folder, "mbd.fasta")
  return(mbd_fasta_filename)
}

#' Create filename: bd_alignment
#' @inheritParams default_params_doc
#' @return name of the bd_alignment file
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_filename.bd_alignment <- function(
  parameters,
  folder_name
)
{
  parameters_folder <- razzo::raz_get_parameters_path(parameters, folder_name = folder_name)
  bd_fasta_filename <- file.path(parameters_folder, "bd.fasta")
  return(bd_fasta_filename)
}
