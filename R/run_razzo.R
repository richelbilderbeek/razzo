#' Runs one \code{razzo} experiment
#' @inheritParams default_params_doc
#' @return a data frame with errors,
#'   with as many rows as model selection parameter sets.
#'   Tip: use \link[pirouette]{pir_plot} to display it.
#'   More important are the files it creates.
#' @author Richel J.C. Bilderbeek
#' @export
run_razzo <- function(
  razzo_params
) {
  check_razzo_params(razzo_params) # nolint raket function
  testit::assert(beastier::is_beast2_installed())

  # Simulate incipient species tree
  if (1 == 2) {
    # Set MBD tree generation seed, #153
    set.seed(razzo_params$misc_params$tree_sim_rng_seed)
  }
  mbd_output <- becosys::bco_mbd_sim(
    mbd_params = razzo_params$mbd_params,
    crown_age = razzo_params$pir_params$experiments[[1]]$inference_model$mrca_prior$mrca_distr$mean$value
  )
  phylogeny <- mbd_output$reconstructed_tree
  # Save phylogeny here, #152
  if (1 == 2) {
    ape::write.tree(
      phy = phylogeny, file = razzo_params$misc_params$tree_filename
    )
  }

  # Let pirouette measure the error
  pirouette::pir_run(
    phylogeny = phylogeny,
    pir_params = razzo_params$pir_params
  )
}
