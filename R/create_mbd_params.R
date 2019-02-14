#' Create mbd parameters
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_mbd_params <- function(
  lambda,
  mu,
  nu,
  q,
  cond = 1,
  crown_age = 10,
  seed = NA
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
  check_mbd_params(mbd_params)
  mbd_params
}

#' Create MBD parameters used in tests
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_test_mbd_params <- function(
) {
  create_mbd_params(
    lambda = 0.1,
    mu = 0.15,
    nu = 0.2,
    q = 0.01,
    crown_age = 15.0,
    cond = 1,
    seed = 1
  )
}
