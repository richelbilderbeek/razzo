#' Opens a parameter file and parses it
#' @inheritParams default_params_doc
#' @return the razzo parameters
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_open_parameters_file <- function(
  parameters_filename
) {
  raz_check_file_exists(parameters_filename)

  # Remove the first column, as it is an unused row name
  parameters <- utils::read.csv(parameters_filename)[, -1]

  testit::assert(parameters$lambda >= 0)
  testit::assert(parameters$mu >= 0)
  testit::assert(parameters$nu >= 0)
  testit::assert(parameters$q >= 0)
  testit::assert(parameters$q <= 1)

  parameters
}
