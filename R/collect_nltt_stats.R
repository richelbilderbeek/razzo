#' @title Collect nltt statistics
#' @description Collect nltt statistics
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and nltt statistics
#' @author Giovanni Laudanno
#' @export
collect_nltt_stats <- function(
  project_folder_name = get_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint
  results_folder <- get_results_path(project_folder_name = project_folder_name)
  nltt_table <- utils::read.csv(
    file.path(results_folder, "nltt_stats.csv")
  )[, -1]
  nltt_table
}
