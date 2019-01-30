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
