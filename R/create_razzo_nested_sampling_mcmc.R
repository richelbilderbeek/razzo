#' Create a nested sampling MCMC that matches the article
#' @export
create_razzo_nested_sampling_mcmc <- function() {
  beautier::create_nested_sampling_mcmc(
    chain_length = 1e6,
    store_every = 1e3,
    particle_count = 1,
    sub_chain_length = 5e3,
    epsilon = 1e-12
  )
}
