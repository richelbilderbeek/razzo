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

  testit::assert(!beautier::is_one_na(razzo_params$pir_params$twinning_params))

  # Save the tree
  tree_filename <- razzo_params$misc_params$tree_filename
  # Create the folder if needed, no warning when the folder is already present
  dir.create(
    path = dirname(tree_filename),
    recursive = TRUE,
    showWarnings = FALSE
  )
  testit::assert(peregrine::is_pff(tree_filename))
  testit::assert(beautier::is_phylo(phylogeny))
  ape::write.tree(
    phy = phylogeny,
    file = tree_filename
  )
  beautier::check_file_exists(tree_filename, "tree_filename")

  # For phylogenies with 5 or less taxa, one cannot use the
  # Coalescent Bayesian Skyline.
  # Remove the pirouette experiments that use CBS
  if (ape::Ntip(phylogeny) < 6) {
    razzo_params$pir_params$experiments <- remove_cbs_exps(
      razzo_params$pir_params$experiments
    )
  }

  # Let pirouette measure the error

  # Create the folder if needed, no warning when the folder is already present
  output_filenames <- pirouette::get_pir_params_filenames(
    razzo_params$pir_params
  )
  for (output_filename in output_filenames) {
    dir.create(
      path = dirname(output_filename), recursive = TRUE, showWarnings = FALSE
    )
  }

  output <- pirouette::pir_run(
    phylogeny = phylogeny,
    pir_params = razzo_params$pir_params
  )
  output
}
