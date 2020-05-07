#' Create mbd parameters
#' @inheritParams default_params_doc
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @aliases create_params_mbd create_mbd_params
#' @export create_params_mbd create_mbd_params
create_params_mbd <- create_mbd_params <- function(
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
#' @author Richèl J.C. Bilderbeek
#' @export
create_test_mbd_params <- function(
  seed = 1
) {
  crown_age <- razzo::create_mbd_params_table()$crown_age[1]
  create_mbd_params(
    lambda = 0.2,
    mu = 0.15,
    nu = 1.0,
    q = 0.1,
    crown_age = crown_age,
    cond = 1,
    seed = seed
  )
}
