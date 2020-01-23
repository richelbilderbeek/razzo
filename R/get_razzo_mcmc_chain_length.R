#' Get the MCMC chain length used in the article
#' @return the MCMC chain length
#' @author Richel J.C. Bilderbeek
#' @export
get_razzo_mcmc_chain_length <- function() {
  # Changed for https://github.com/richelbilderbeek/razzo/issues/346
  # Original value: 1e7
  1e8
}
