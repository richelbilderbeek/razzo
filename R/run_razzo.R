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
  # Note: if speciation rates are zero, PBD::mbd_sim will last forever
  set.seed(razzo_params$tree_sim_rng_seed)
  mbd_output <- becosys::bco_mbd_sim(
    mbd_params = razzo_params$mbd_params,
    crown_age = razzo_params$inference_params$mrca_prior$mrca_distr$mean$value
  )
  phylogeny <- mbd_output$reconstructed_tree

  # Let pirouette measure the error
  pirouette::pir_run(
    phylogeny = phylogeny,
    twinning_params = razzo_params$twinning_params,
    alignment_params = razzo_params$alignment_params,
    model_select_params = razzo_params$model_select_params,
    inference_params = razzo_params$inference_params,
    error_measure_params = razzo_params$error_measure_params
  )
}
