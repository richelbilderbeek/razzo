#' Check if the \code{razzo_params} are valid razzo parameters.
#'
#' Will \link{stop} if the \code{razzo_params} are invalid,
#' else will do nothing
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
check_razzo_params <- function(
  razzo_params
) {
  argument_names <- c(
    "mbd_params",
    "twinning_params",
    "alignment_params",
    "gen_model_select_params",
    "best_model_select_params",
    "inference_params",
    "sampling_method",
    "misc_params"
  )
  for (arg_name in argument_names) {
    if (!arg_name %in% names(razzo_params)) {
      stop(
        "'", arg_name, "' must be an element of a 'razzo_params'"
      )
    }
  }

  becosys::check_mbd_params(razzo_params$mbd_params)
  pirouette:::check_twinning_params(razzo_params$twinning_params) # nolint internal pirouette function, will be exported in pirouette v1.1
  pirouette:::check_alignment_params(razzo_params$alignment_params) # nolint internal pirouette function, will be exported in pirouette v1.1
  pirouette:::check_model_select_params(razzo_params$gen_model_select_params) # nolint internal pirouette function, will be exported in pirouette v1.1
  pirouette:::check_model_select_params(razzo_params$best_model_select_params) # nolint internal pirouette function, will be exported in pirouette v1.1
  pirouette:::check_inference_param(razzo_params$inference_params) # nolint internal pirouette function, will be exported in pirouette v1.1
  # testit::assert( # nolint
  #   razzo_params$sampling_method %in% razzo::rkt_get_sampling_methods() # nolint
  # ) # nolint
}
