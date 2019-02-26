#' @title Collect esses
#' @description Collect esses
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and esses
#' @author Giovanni Laudanno
#' @export
collect_esses <- function(
  project_folder_name = get_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint
  results_folder <- get_results_path(project_folder_name = project_folder_name)
  esses <- utils::read.csv(
    file.path(results_folder, "esses.csv")
  )[, -1]
  esses
}
