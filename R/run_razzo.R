#' Runs one \code{razzo} experiment
#' @inheritParams default_params_doc
#' @return a data frame with errors,
#'   with as many rows as model selection parameter sets.
#'   Tip: use \link[pirouette]{pir_plot} to display it.
#'   More important are the files it creates.
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
run_razzo <- function(
  razzo_params,
  experiments = list(pirouette::create_experiment())
) {
  check_razzo_params(razzo_params) # nolint razzo function
  testit::assert(beastier::is_beast2_installed())

  # Simulate incipient species tree
<<<<<<< HEAD
  if (1 == 2) {
    # Set MBD tree generation seed, #153
    set.seed(razzo_params$misc_params$tree_sim_rng_seed)
  }
  mbd_output <- becosys::bco_mbd_sim(
    mbd_params = razzo_params$mbd_params,
    crown_age = razzo_params$pir_params$experiments[[1]]$inference_model$mrca_prior$mrca_distr$mean$value
=======
  mbd_output <- mbd::mbd_sim(
    pars = c(
      razzo_params$mbd_params$lambda,
      razzo_params$mbd_params$mu,
      razzo_params$mbd_params$nu,
      razzo_params$mbd_params$q
    ),
    n_0 = 2,
    age = razzo_params$mbd_params$crown_age,
    cond = razzo_params$mbd_params$cond,
    seed = razzo_params$mbd_params$seed
>>>>>>> develop
  )
  phylogeny <- mbd_output$reconstructed_tree

  directory <- dirname(razzo_params$twinning_params$twin_tree_filename)
  tree_filename <- file.path(
    directory,
    razzo_params$misc_params$tree_filename
  )
  ape::write.tree(
    phy = phylogeny, file = tree_filename
  )
  testit::assert(file.exists(tree_filename))

  # Let pirouette measure the error
  pirouette::pir_run(
    phylogeny = phylogeny,
<<<<<<< HEAD
    pir_params = razzo_params$pir_params
=======
    twinning_params = razzo_params$twinning_params,
    alignment_params = razzo_params$alignment_params,
    model_select_params = razzo_params$model_select_params,
    inference_params = razzo_params$inference_params,
    error_measure_params = razzo_params$error_measure_params,
    experiments = experiments
>>>>>>> develop
  )
}
