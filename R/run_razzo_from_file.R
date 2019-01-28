#' Run a razzo experiment from a filename
#' @param razzo_input_filename name of a file that contains
#'   a \code{raket_params},
#'   as can be created by \link{create_razzo_params}
#' @author Richel J.C. Bilderbeek
#' @export
run_razzo_from_file <- function(razzo_input_filename) {
  if (!file.exists(razzo_input_filename)) {
    stop(
      "'razzo_input_filename' cannot be found. Value: ",
      razzo_input_filename
    )
  }
  razzo_params <- readRDS(razzo_input_filename)
  check_razzo_params(razzo_params)
  run_razzo(razzo_params)
}
