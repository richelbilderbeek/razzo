#' Estimates the marginal likelihood of a model
#' (as specified in \code{parameters})
#' for an alignment.
#' @inheritParams default_params_doc
#' @return a list with the following elements:
#' \itemize{
#'   \item log_lik the log of the estimated marginal likelihood
#'   \item log_error the ?log of the error in that estimation
#' }
#' @author Richel J.C. Bilderbeek
raz_est_marg_lik <- function(
  parameters,
  alignment
) {
  list(
    log_lik = -1.0,
    log_error = 1.0
  )
}
