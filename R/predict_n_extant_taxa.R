#' Calculate the expected number of extant taxa
#' at the present from the MBD parameters
#' @param pars numeric vector of length four, with values for
#' MBD parameters of lambda, mu, nu and q respectively
#' @param the crown age, time between the present and the
#' time there were only two lineages
#' @return the expected number of extant taxa
#' @author Richel J.C. Bilderbeek
predict_n_extant_taxa <- function(pars, crown_age)
{
  testit::assert(length(pars) == 4)
  lambda <- pars[1]
  mu <- pars[2]
  nu <- pars[3]
  q <- pars[4]
  testit::assert(lambda >= 0.0)
  testit::assert(mu >= 0.0)
  testit::assert(nu >= 0.0)
  testit::assert(q >= 0.0)
  testit::assert(q <= 1.0)
  0
}
