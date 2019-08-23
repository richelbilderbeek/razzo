#' Create a nested sampling MCMC that matches the article
#' @export
create_razzo_nested_sampling_mcmc <- function() {
  est_evidence_mcmc = beautier::create_nested_sampling_mcmc(
    chain_length = 10^6,
    store_every = 10^3,
    particle_count = 1,
    sub_chain_length = 5.0*10^3,
    epsilon = 10^-12
  )
}
