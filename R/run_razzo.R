#' Runs one \code{razzo} experiment
#' @inheritParams default_params_doc
#' @return a data frame with errors,
#'   with as many rows as model selection parameter sets.
#'   Tip: use \link[pirouette]{pir_plot} to display it.
#'   More important are the files it creates.
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
run_razzo <- function(
  razzo_params
) {
  check_razzo_params(razzo_params) # nolint razzo function
  testit::assert(beastier::is_beast2_installed())

  # Simulate incipient species tree
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
  )
  phylogeny <- mbd_output$reconstructed_tree

  directory <- dirname(
    razzo_params$pir_params$twinning_params$twin_tree_filename
  )
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
    pir_params = razzo_params$pir_params
  )
}
