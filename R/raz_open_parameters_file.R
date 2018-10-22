#' Opens a parameter file and parses it
#' @inheritParams default_params_doc
#' @return the razzo parameters
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_open_parameters_file <- function(
  parameters_filename
) {
  testit::assert(file.exists(parameters_filename))

  reading <- utils::read.csv(parameters_filename)
  parameters <- data.frame(t(reading[, 2]))
  names(parameters) <- reading[, 1]

  testit::assert(parameters$lambda >= 0)
  testit::assert(parameters$mu >= 0)
  testit::assert(parameters$nu >= 0)
  testit::assert(parameters$q >= 0)
  testit::assert(parameters$q <= 1)

  return(parameters)
}
