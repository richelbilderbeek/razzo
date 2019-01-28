#' @title Get paths for the results
#' @description Get paths for the results
#' @inheritParams default_params_doc
#' @return the paths of the results
#' @author Giovanni Laudanno
#' @export
get_results_path <- function(
  project_folder_name
) {

  check_project_folder_name(project_folder_name) # nolint

  results_folder <- file.path(
    project_folder_name,
    "results"
  )

  results_folder
}
