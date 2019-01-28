#' Creates a list of mbd parameters,
#' @inheritParams default_params_doc
#' @return a list of parameters
#' @export
#' @author Giovanni Laudanno
create_mbd_params <- function(
  lambda,
  mu,
  nu,
  q,
  cond,
  crown_age,
  seed
) {
  mbd_params <- list(
    lambda = lambda,
    mu = mu,
    nu = nu,
    q = q,
    cond = cond,
    crown_age = crown_age,
    seed = seed
  )
  becosys::check_mbd_params(mbd_params) # nolint razzo function
  mbd_params
}
