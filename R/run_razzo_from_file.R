#' Run a razzo experiment from a filename
#' @inheritParams default_params_doc
#' @param parameters_filename name of a file that contains
#'   a \code{razzo_params},
#'   as can be created by \link{create_razzo_params}
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
run_razzo_from_file <- function(
  parameters_filename
) {
  if (!file.exists(parameters_filename)) {
    stop(
      "'parameters_filename' cannot be found. Value: ",
      parameters_filename
    )
  }
  razzo_params <- readRDS(parameters_filename)
  check_razzo_params(razzo_params)
  run_razzo(
    razzo_params = razzo_params
  )
}
