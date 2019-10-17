#' Get the MCMC sampling interval used in the article
#' @return the MCMC sampling interval
#' @author Richel J.C. Bilderbeek
#' @export
get_razzo_mcmc_store_every <- function() {
  get_razzo_mcmc_chain_length() / 1000
}
