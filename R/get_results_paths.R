#' @title Get paths for the results
#' @description Get paths for the results
#' @inheritParams default_params_doc
#' @return the paths of the results
#' @author Giovanni Laudanno
#' @export
get_results_paths <- function(
  project_folder_name
) {

  if (basename(project_folder_name) != "razzo_project") {
    stop("'project_folder_name' must end with 'razzo_project'")
  }
  if (!dir.exists(project_folder_name)) {
    stop("'project_folder_name' absent")
  }

  results_folder <- file.path(
    project_folder_name,
    "results_folder"
  )

  if (!(dir.exists(results_folder))) {
    stop("'results' folder does not exist!")
  }

  results_folder
}
