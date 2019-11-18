#' Get the MCMC chain used in the article
#' @inheritParams default_params_doc
#' @return the MCMC chain
#' @author Richel J.C. Bilderbeek
#' @export
get_razzo_mcmc <- function(
  model_type,
  folder_name = peregrine::get_pff_tempfile()
) {

  if (model_type == "generative") {
    tracelog_filename <- file.path(folder_name, "mbd_gen.log")
    treelog_filename <- file.path(folder_name, "mbd_gen.trees")
  } else {
    testit::assert(model_type = "candidate")
    tracelog_filename <- file.path(folder_name, "mbd_best.log")
    treelog_filename <- file.path(folder_name, "mbd_best.trees")
  }
  beautier::create_mcmc(
    chain_length = get_razzo_mcmc_chain_length(),
    store_every = get_razzo_mcmc_store_every(),
    tracelog = create_tracelog(
      filename = tracelog_filename
    ),
    treelog = create_treelog(
      filename = treelog_filename
    )
  )
}
