#' Create a nested sampling MCMC that matches the article
#' @export
create_razzo_nested_sampling_mcmc <- function() { # nolint indeed a long function name
  beautier::create_nested_sampling_mcmc(
    chain_length = get_razzo_mcmc_chain_length(),
    store_every = get_razzo_mcmc_store_every(),
    particle_count = 1,
    sub_chain_length = 5e3,
    epsilon = 1e-12
  )
}
