#' Collect all data
#' in one \code{.csv} file called \code{results/all_data.csv}
#' @inheritParams default_params_doc
#' @param new_run If true will collect all data from scratch
#' @return the name of the file created
#' @author Giovanni Laudanno
#' @aliases create_file_all_data create_all_data_file
#' @export create_file_all_data create_all_data_file
create_file_all_data <- create_all_data_file <- function(
  project_folder_name = getwd(),
  new_run = FALSE
) {
  razzo::check_project_folder_name(project_folder_name) # nolint

  df <- razzo::collect_all_data(
    project_folder_name = project_folder_name,
    new_run = new_run
  )
  filename <- file.path(project_folder_name, "results", "all_data.csv")
  dir.create(path = dirname(filename), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(
    x = df,
    file = filename
  )
  filename
}
