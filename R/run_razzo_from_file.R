#' Run a razzo experiment from a filename
#' @inheritParams default_params_doc
#' @param parameters_filename name of a file that contains
#'   a \code{razzo_params},
#'   as can be created by \link{create_params_razzo}
#' @param add_verbose if \code{TRUE}, this will set verbosity
#'   to \code{TRUE}, even though the \code{razzo} parameter
#'   file has set it to \code{FALSE}. This is used in debugging
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
run_razzo_from_file <- function(
  parameters_filename,
  add_verbose = FALSE
) {
  if (!file.exists(parameters_filename)) {
    stop(
      "'parameters_filename' cannot be found. Value: ",
      parameters_filename
    )
  }
  if (!is_one_bool(add_verbose)) {
    stop("'add_verbose' must be one boolean")
  }

  razzo_params <- readRDS(parameters_filename)
  check_razzo_params(razzo_params)
  if (add_verbose == TRUE) {
    razzo_params$pir_params$verbose <- TRUE
  }
  run_razzo(
    razzo_params = razzo_params
  )
}
