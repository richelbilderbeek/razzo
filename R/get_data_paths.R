#' @title Get paths for all the settings
#' @description Get paths for all the settings
#' @inheritParams default_params_doc
#' @return the paths of all the parameter settings
#' @examples
#'   # Obtain the paths of all razzo testing folders
#'   all_paths <- get_data_paths(get_path("razzo_project"))
#'
#'   # In each of these, there is a 'parameters.RDa' file
#'   parameter_files <- file.path(all_paths, "parameters.RDa")
#'   testthat::expect_true(all(file.exists(parameter_files)))
#' @author Giovanni Laudanno
#' @export
get_data_paths <- function(
  project_folder_name
) {

  check_project_folder_name(project_folder_name) # nolint

  data_folder <- file_path(
    project_folder_name,
    "data"
  )

  all_settings <- c()
  par_settings_folders <- file_path(data_folder, list.files(data_folder))
  for (p in par_settings_folders) {
    seed_folders <- file_path(p, list.files(p))
    all_settings <- c(all_settings, seed_folders)
  }
  all_settings
}
