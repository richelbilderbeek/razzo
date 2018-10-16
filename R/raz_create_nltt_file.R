#' Create the NLTT statistics distribution files from a posterior file.
#' Assumes for a posterior file named '1x.trees'
#'   that there is a 'true' tree file called '1x.tree'
#' @inheritParams default_params_doc
#' @return name of the nLTT file created
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_nltt_file <- function(trees_filename) {

  # TODO: trees_filename should exist one day :-)
  if (1 == 2) {
    testit::assert(file.exists(trees_filename))
  }

  base <- tools::file_path_sans_ext(trees_filename)
  nltt_filename <- paste0(c(base, "_nltts.csv"), collapse = "")
  tree_filename <- paste0(c(base, ".tree"), collapse = "")
  log_filename <- paste0(c(base, ".log"), collapse = "")

  # The true tree
  tree <- ape::read.tree(file = tree_filename)
  # The posterior
  posterior <- tracerer::parse_beast_posterior(
    trees_filename,
    log_filename
  )

  # TODO: create the nLTT file
  # Perhaps already work
  if (1 == 2) {
    nltt_diff <- rep(NA, length(posterior$trees))
    for (i in 1:length(posterior$trees))
    {
      nltt_diff[i] <- nLTT::nLTTstat(tree, posterior$trees[[i]]) # nolint nLTT used older coding standard
    }
    utils::write.csv(x = nltt_diff, file = nltt_filename)
  }

  nltt_filename
}
