#' @noRd
file_path <- function(..., fsep = .Platform$file.sep){
  gsub("//", "/", file.path(..., fsep = fsep))
}


#' Get the full path of a file in the \code{inst/extdata} folder
#' @inheritParams default_params_doc
#' @return the full path of the filename, if and only if
#'   the file is present. Will stop otherwise.
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @examples
#'   testit::assert(is.character(get_razzo_path("parameters.RDa")))
#' @export
get_razzo_path <- function(filename) {
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

#' @title Get paths for all the settings
#' @description Get paths for all the settings
#' @inheritParams default_params_doc
#' @return the paths of all the parameter settings
#' @examples
#'   # Obtain the paths of all razzo testing folders
#'   all_paths <- get_data_paths(get_razzo_path("razzo_project"))
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
  oldskool_paths <- all_settings


  paths <- dirname(
    list.files(
      path = project_folder_name,
      pattern = "parameters.RDa",
      recursive = TRUE,
      full.names = TRUE
    )
  )
  testit::assert(paths == oldskool_paths)
  paths

}
