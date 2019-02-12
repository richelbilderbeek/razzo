#' Create the parameter interval for mbd setting
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
create_mbd_params_interval <- function(
  lambda = 0.2,
  mu = 0.15,
  nu = c(1.0, 1.5, 2.0),
  q = c(0.1, 0.15, 0.2),
  seed = seq(from = 1, to = 2, by = 1),
  crown_age = 15.0,
  cond = 1
) {
  lambda <- unique(lambda)
  mu <- unique(mu)
  nu <- unique(nu)
  q <- unique(q)
  seed <- unique(seed)
  mbd_params_interval <- expand.grid(
    lambda = lambda,
    mu = mu,
    nu = nu,
    q = q,
    seed = seed,
    crown_age = crown_age,
    cond = cond
  )
  testit::assert(
   length(crown_age) == 1
  )
  testit::assert(
   length(cond) == 1
  )
  mbd_params_interval
}
