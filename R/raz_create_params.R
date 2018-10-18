#' Creates a list of parameters,
#' after checking the arguments for their validity
#' @inheritParams default_params_doc
#' @return a list of parameters
#' @export
#' @author Richel J.C. Bilderbeek
raz_create_params <- function(lambda) {
  # TODO: add more arguments

    if (lambda < 0.0) {
    stop("'lambda' must be positive")
  }
  # TODO: check all other arguments

  params <- list()
  params$lambda <- lambda
  # TODO: add other values
  params
}
