#' Run a razzo experiment from a filename
#' @param parameters_filename name of a file that contains
#'   a \code{razzo_params},
#'   as can be created by \link{create_params_razzo}
#' @param add_verbose if \code{TRUE}, this will set verbosity
#'   to \code{TRUE}, even though the \code{razzo} parameter
#'   file has set it to \code{FALSE}. This is used in debugging
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
run_razzo_from_file <- function(
  parameters_filename,
  add_verbose = FALSE
) {
  beautier::check_file_exists(parameters_filename, "parameters_filename")

  if (!beautier::is_one_bool(add_verbose)) {
    stop("'add_verbose' must be one boolean")
  }

  razzo_params <- readRDS(parameters_filename)
  razzo::check_razzo_params(razzo_params)
  if (add_verbose == TRUE) {
    razzo_params$pir_params$verbose <- TRUE
    for (i in seq_along(razzo_params$pir_params$experiments)) {
      razzo_params$pir_params$experiments[[i]]$beast2_options$verbose <- TRUE
    }
  }
  razzo::run_razzo(
    razzo_params = razzo_params
  )
}
