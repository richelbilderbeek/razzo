#' Calculate the number of taxa that trees will have when
#' simulated from the set of MBD parameters.
#'
#' @inheritParams default_params_doc
#' @return a numeric vector of whole numbers,
#'   with as much values as the \code{mbd_paramses},
#'   containing the number of taxa each MBD parameter set will create
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
calc_n_taxas <- function(
  mbd_paramses
) {
  razzo::check_mbd_paramses(mbd_paramses)

  # STUB! These are just rounded off random values
  # Instead, simulate per MBD params set an MBD tree and
  # count the number of taxa
  n_0 <- 2
  x <- data.frame(matrix(
    unlist(mbd_paramses),
    ncol = length(mbd_paramses[[1]]),
    byrow = TRUE
  ))
  colnames(x) <- names(mbd_paramses[[1]])
  x <- dplyr::distinct(x)
  n_taxas <- rep(NA, nrow(x))
  for (m in seq_len(nrow(x))) {
    pars <- x[m, ]
    brts <- mbd::mbd_sim(
      pars = c(pars$lambda, pars$mu, pars$nu, pars$q),
      n_0 = n_0,
      age = pars$crown_age,
      cond = pars$cond,
      seed = pars$seed
    )$brts
    n_taxas[m] <- length(brts) + n_0 - 1
  }
  testit::assert(length(mbd_paramses) == length(n_taxas))
  n_taxas
}
