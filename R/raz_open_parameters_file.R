#' Opens a parameter file and parses it
#' @inheritParams default_params_doc
#' @param parameter_filename name of the parameter filename,
#'   for example '/my_folder/1/parameters.csv'
#' @return the razzo parameters
#' @author Richel J.C. Bilderbeek
#' @export
raz_open_parameters_file <- function(parameter_filename)
{
  # TODO: actually read the file
  if (1 == 2) {
    testit::assert(file.exists(parameter_filename))
  }

  parameters <- list()
  parameters$lambda <- 1.0
  parameters
}
