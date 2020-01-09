#' Clean up a path by removing double folder seperators
#' @param filename name of a file
#' @author Richel J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#'  if (rappdirs::app_dir()$os != "win") {
#'    expect_equal(clean_path("a//b"), "a/b")
#'  }
#'  expect_error(clean_path(NULL))
#'  expect_error(clean_path(NA))
#'  expect_error(clean_path(Inf))
#' @export
clean_path <- function(filename) {
  assertive::assert_is_a_string(filename)
  mbd::file_path(filename)
}

#' Clean up a paths by removing double folder seperators
#' @param filenames names of files
#' @author Richel J.C. Bilderbeek
#' @examples
#' library(testthat)
#' if (rappdirs::app_dir()$os != "win") {
#'   expect_equal(
#'     razzo::clean_paths(c("a//b")),
#'     c("a/b")
#'   )
#'   expect_equal(
#'     razzo::clean_paths(c("a//b", "c//d")),
#'     c("a/b", "c/d")
#'   )
#' }
#' expect_error(razzo::clean_paths(NULL))
#' expect_error(razzo::clean_paths(NA))
#' expect_error(razzo::clean_paths(Inf))
#' @export
clean_paths <- function(filenames) {
  assertive::assert_is_not_null(filenames)
  for (i in seq_along(filenames)) {
    assertive::assert_is_a_string(filenames[i])
    filenames[i] <- razzo::clean_path(filenames[i])
  }
  filenames
}


#' Get the full path of a file in the \code{inst/extdata} folder
#' @inheritParams default_params_doc
#' @return the full path of the filename, if and only if
#'   the file is present. Will stop otherwise.
#' @examples
#' library(testthat)
#'
#' expect_true(file.exists(get_razzo_path("parameters.RDa")))
#' expect_error(get_razzo_path("abs.ent"))
#' @export
get_razzo_path <- function(filename) {
  raztr::get_raztr_path(filename)
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

  razzo::check_project_folder_name(project_folder_name) # nolint
  razzo::clean_path(
    file.path(
      project_folder_name,
      "results"
    )
  )
}

#' Get the folder paths of \code{razzo} experiments
#'
#' This function looks recursively for \code{parameters.RDa} files
#' and return the folder names of these.
#' @inheritParams default_params_doc
#' @param full_names TRUE if you want to return the full path
#' @return the folder paths of all \code{razzo} experiments
#' @examples
#' library(testthat)
#'
#' # Obtain the paths of all razzo testing folders
#' all_paths <- get_data_paths(get_razzo_path("razzo_project"))
#'
#' # In each of these, there is a 'parameters.RDa' file
#' parameter_files <- file.path(all_paths, "parameters.RDa")
#' expect_true(all(file.exists(parameter_files)))
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
get_data_paths <- function(
  project_folder_name,
  full_names = TRUE
) {
  razzo::check_project_folder_name(project_folder_name) # nolint
  razzo::clean_paths(
    dirname(
      list.files(
        path = project_folder_name,
        pattern = "parameters.RDa",
        recursive = TRUE,
        full.names = full_names
      )
    )
  )
}
