#' Create mbd parameters
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
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
#' @author Richel J.C. Bilderbeek
#' @aliases create_test_mbd_params create_params_mbd_test
#' @export create_test_mbd_params create_params_mbd_test
create_test_mbd_params <- create_params_mbd_test <- function(
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

#' Create the parameter interval for mbd setting
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @aliases create_mbd_params_interval create_paramses_mbd
#' @export create_mbd_params_interval create_paramses_mbd
create_mbd_params_interval <- create_paramses_mbd <- function(
  lambda = 0.2,
  mu = c(0, 0.15),
  nu = c(1.0, 1.5, 2.0, 2.5),
  q = c(0.1, 0.15, 0.2),
  n_replicates = 2,
  crown_age = 15.0,
  cond = 1
) {
  lambda <- unique(lambda)
  mu <- unique(mu)
  nu <- unique(nu)
  q <- unique(q)
  seed <- 1:n_replicates
  mbd_paramses <- expand.grid(
    seed = seed,
    lambda = lambda,
    mu = mu,
    nu = nu,
    q = q,
    crown_age = crown_age,
    cond = cond
  )
  seeds <- 1:nrow(mbd_paramses)
  mbd_paramses$seed <- NULL
  mbd_paramses$seed <- seeds
  testit::assert(
   length(crown_age) == 1
  )
  testit::assert(
   length(cond) == 1
  )
  mbd_paramses
}
