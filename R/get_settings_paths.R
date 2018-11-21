#' @title Get paths for all the settings
#' @description Get paths for all the settings
#' @inheritParams default_params_doc
#' @return the paths of all the parameter settings
#' @author Giovanni Laudanno
#' @export
get_settings_paths <- function(
  project_folder_name
) {

  if (basename(project_folder_name) != "razzo_project") {
    stop("'project_folder_name' must end with 'razzo_project'")
  }
  if (!dir.exists(project_folder_name)) {
    stop("'project_folder_name' absent")
  }

  data_folder <- file.path(
    project_folder_name,
    "data"
  )

  all_settings <- c()
  par_settings_folders <- file.path(data_folder, list.files(data_folder))
  for (p in par_settings_folders) {
    seed_folders <- file.path(p, list.files(p))
    for (s in seed_folders) {
      model_folders <- file.path(s, list.files(s))
      all_settings <- c(all_settings, model_folders)
    }
  }
  all_settings
}
