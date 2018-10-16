#' Create the NLTT statistics distribution files from a posterior file.
#' Assumes for a posterior file named '1x.trees'
#'   that there is a 'true' tree file called '1x.tree'
#' @inheritParams default_params_doc
#' @return nltt statistics
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_nltt_file <- function(parameters, folder_name) {

  parameters_folder <- raz_get_parameters_path(parameters, folder_name)
  mbd_tree_filename <- file.path(parameters_folder, "mbd.tree")
  bd_tree_filename <- file.path(parameters_folder, "bd.tree")
  testit::assert(file.exists(mbd_tree_filename))
  testit::assert("mbd.tree" %in% list.files(parameters_folder))
  testit::assert(length(mbd_tree_filename) > 0)
  if (1 == 2) { #TODO: we can use these checks only after we have a way to simulate bd.tree
  testit::assert(file.exists(bd_tree_filename))
  testit::assert("bd.tree" %in% list.files(parameters_folder))
  testit::assert(length(bd_tree_filename) > 0)
  }

  trees_filename <- c(mbd_tree_filename, bd_tree_filename)

  bd.base <- tools::file_path_sans_ext(bd_tree_filename)
  bd.nltt_filename <- paste0(c(bd.base, "_nltts.csv"), collapse = "")
  # bd.tree_filename <- paste0(c(bd.base, ".tree"), collapse = "")
  bd.log_filename <- paste0(c(bd.base, ".log"), collapse = "")

  mbd.base <- tools::file_path_sans_ext(bd_tree_filename)
  mbd.nltt_filename <- paste0(c(mbd.base, "_nltts.csv"), collapse = "")
  # tree_filename <- paste0(c(mbd.base, ".tree"), collapse = "")
  mbd.log_filename <- paste0(c(mbd.base, ".log"), collapse = "")

  # The true tree
  # bd.tree <- ape::read.tree(file = bd_tree_filename)
  # mbd.tree <- ape::read.tree(file = mbd_tree_filename)

  bd.sim <- get(load(
    bd_tree_filename
  ))
  bd.tree <- bd.sim$bd_tree

  mbd.sim <- get(load(
    mbd_tree_filename
  ))
  mbd.tree <- mbd.sim$tes

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
