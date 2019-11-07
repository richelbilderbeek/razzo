#' Get the MCMC chain used in the article
#' @return the MCMC chain
#' @author Richel J.C. Bilderbeek
#' @export
get_razzo_mcmc <- function() {
  beautier::create_mcmc(
    chain_length = get_razzo_mcmc_chain_length(),
    store_every = get_razzo_mcmc_store_every(),
    tracelog = beautier::create_tracelog(
      filename = get_tracelog_filename()
    ),
    treelog = beautier::create_treelog(
      filename = get_treelog_filename()
    )
  )
}
