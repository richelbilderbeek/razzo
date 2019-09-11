#' Calculate the number of taxa that trees will have when
#' simulated from the set of MBD parameters.
#'
#' @inheritParams default_params_doc
#' @return a numeric vector of whole numbers,
#'   with as much values as the \code{mbd_paramses},
#'   containing the number of taxa each MBD parameter set will create
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
calc_n_taxas <- function(mbd_paramses) {
  check_mbd_paramses(mbd_paramses)

  # STUB! These are just rounded off random values
  # Instead, simulate per MBD params set an MBD tree and
  # count the number of taxa
  n_taxa <- round(stats::runif(n = length(mbd_paramses), min = 2, max = 1000))
  n_0 <- 2
  max_seed <- 1e2
  for (m in seq_along(mbd_paramses)) {
    pars <- mbd_paramses[[m]]
    n_taxa_seed <- rep(NA, max_seed)
    for (seed in 1:max_seed) {
      brts <- mbd::mbd_sim(
        pars = c(pars$lambda, pars$mu, pars$nu, pars$q),
        n_0 = n_0,
        age = pars$crown_age,
        cond = pars$cond,
        seed = pars$seed
      )$brts
      n_taxa_seed[seed] <- length(x$brts) + n_0 - 1
    }
    out <- n_taxa_seed
  }
  mbd_paramses

  # Simple data structure
  testit::assert(length(mbd_paramses) == length(n_taxa))
  n_taxa
}
