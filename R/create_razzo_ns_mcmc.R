#' Create a nested sampling MCMC that matches the article
#' @inheritParams default_params_doc
#' @param index the index of the candidate model. The first
#'   candidate model has an index of 1, also when there is yes/no a
#'   generative model.
#' @export
create_razzo_ns_mcmc <- function(
  folder_name,
  model_type,
  index = 1
) { # nolint indeed a long function name
  mcmc <- beautier::create_ns_mcmc(
    chain_length = get_razzo_mcmc_chain_length(),
    store_every = get_razzo_mcmc_store_every(),
    particle_count = 1,
    sub_chain_length = 5e3,
    epsilon = 1e-12
  )
  if (model_type == "generative") {
    mcmc$tracelog$filename <- file.path(folder_name, "mbd_gen_evidence.log")
    mcmc$treelog$filename <- file.path(folder_name, "mbd_gen_evidence.trees")
  }
  else {
    testit::assert(model_type == "candidate")
    mcmc$tracelog$filename <- file.path(folder_name, paste0("mbd_best_", index, "_evidence.log"))
    mcmc$treelog$filename <- file.path(folder_name, paste0("mbd_best_", index, "_evidence.trees"))
  }
  mcmc
}
