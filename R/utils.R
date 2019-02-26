#' Function to check if a file exists.
#' Calls \code{stop} if the file is absent
#' @param filename name of the file
#' @return nothing. Will \code{stop} if the file is absent,
#'   with a proper error message
#' @author Richel J.C. Bilderbeek
#' @noRd
check_file_exists <- function(filename) {
  if (!file.exists(filename)) {
    stop(
      "File not found. Could not find file with path '",
      filename,
      "'"
    )
  }
}

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
      "'project_folder_name' absent. ",
      "Folder with path '", project_folder_name, "' not found"
    )
  }
}

#' Get the full path of a file in the \code{inst/extdata} folder
#' @inheritParams default_params_doc
#' @return the full path of the filename, if and only if
#'   the file is present. Will stop otherwise.
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'   testit::assert(is.character(get_path("parameters.RDa")))
#' @export
get_path <- function(filename) {
  full <- system.file("extdata", filename, package = "razzo")
  if (!file.exists(full)) {
    stop("'filename' must be the name of a file in 'inst/extdata'")
  }
  full
}

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

#' Opens a parameter file and parses it
#' @inheritParams default_params_doc
#' @return the razzo parameters
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
open_parameters_file <- function(
  parameters_filename
) {
  check_file_exists(parameters_filename) # nolint internal function

  parameters <- NULL
  if (tools::file_ext(parameters_filename) == "RDa") {
    parameters <- readRDS(parameters_filename)
    check_razzo_params(parameters)
  } else {
    # Remove the first column, as it is an unused row name
    parameters <- utils::read.csv(parameters_filename)[, -1]

    testit::assert(parameters$lambda >= 0)
    testit::assert(parameters$mu >= 0)
    testit::assert(parameters$nu >= 0)
    testit::assert(parameters$q >= 0)
    testit::assert(parameters$q <= 1)
  }
  parameters
}
