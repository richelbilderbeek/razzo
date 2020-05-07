#' Collect all MBD parameters of a \code{razzo} run and put these
#' in one \code{.csv} file called \code{results/mbd_params.csv}
#' @inheritParams default_params_doc
#' @return the name of the file created
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @aliases create_file_mbd_params create_mbd_params_file
#' @export create_file_mbd_params create_mbd_params_file
create_file_mbd_params <- create_mbd_params_file <- function(
  project_folder_name = getwd()
) {
  check_project_folder_name(project_folder_name) # nolint

  df <- collect_mbd_params(project_folder_name) # nolint razzo function
  filename <- file.path(project_folder_name, "results", "mbd_params.csv")
  dir.create(path = dirname(filename), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(
    x = df,
    file = filename
  )
  filename
}
