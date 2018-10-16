#' Create all parameters files in a folder.
#' In that folder, one folder is created per parameter file.
#' In each subfolder, a file 'parameters.csv' is created
#' @inheritParams default_params_doc
#' @return full paths of the files created
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_parameters_files <- function(folder_name) {

  # * folder_name (in our case, it's called 'data')
  #    * 1
  #      * parameters.csv
  #    * 2
  #      * parameters.csv
  # Etcetera

  # TODO: Create one sub-folder per parameter set
  sub_folder_name <- "1"
  local_path <- file.path(folder_name, sub_folder_name)
  path <- file.path(local_path, "parameters.csv")

  # TODO: Create the parameters file here

  # Return the path to the file
  path
}
