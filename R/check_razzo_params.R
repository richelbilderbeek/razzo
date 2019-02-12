#' Check if the \code{razzo_params} are valid razzo parameters.
#'
#' Will \link{stop} if the \code{razzo_params} are invalid,
#' else will do nothing
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
check_razzo_params <- function(
  razzo_params
) {
  argument_names <- c(
    "mbd_params", "pir_params"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(razzo_params)) {
      stop(
        "'", arg_name, "' must be an element of a 'razzo_params'"
      )
    }
  }

  # becosys::check_mbd_params(razzo_params$mbd_params) # nolint Issue #150
  pirouette::check_pir_params(razzo_params$pir_params) # nolint internal pirouette function, will be exported in pirouette v1.1
}
