#' Check if \code{project_folder_name} ends with \code{razzo_project}
#' and is the name of an existing folder.
#' Will throw if not.
#' Else will do nothing.
#' @return nothing
#' @author Richel J.C. Bilderbeek
#' @noRd
check_project_folder_name <- function(project_folder_name) {
  if (basename(project_folder_name) != "razzo_project") {
    stop("'project_folder_name' must end with 'razzo_project'")
  }
  if (!dir.exists(project_folder_name)) {
    stop(
      "'project_folder_name' is absent. ",
      "Folder with path '", project_folder_name, "' not found"
    )
  }
}
