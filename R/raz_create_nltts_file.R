#' Create the NLTT statistics distribution files from a posterior file.
#' Assumes for a posterior file named '1x.trees'
#'   that there is a 'true' tree file called '1x.tree'
#' @inheritParams default_params_doc
#' @return nltt statistics
#' @author Richel J.C. Bilderbeek
#' @export
raz_create_nltts_file <- function(
  parameters,
  folder_name
) {
  parameters_folder <- raz_get_parameters_path(parameters, folder_name)
  mbd_tree_filename <- file.path(parameters_folder, "mbd.tree")
  bd_tree_filename <- file.path(parameters_folder, "bd.tree")
  testit::assert(file.exists(mbd_tree_filename))
  testit::assert("mbd.tree" %in% list.files(parameters_folder))
  testit::assert(length(mbd_tree_filename) > 0)
  # TODO: we can use these checks only after we have a way to simulate bd_tree
  if (1 == 2) {
    testit::assert(file.exists(bd_tree_filename))
    testit::assert("bd_tree" %in% list.files(parameters_folder))
    testit::assert(length(bd_tree_filename) > 0)
  }

  trees_filename <- c(mbd_tree_filename, bd_tree_filename)

  bd_base <- tools::file_path_sans_ext(bd_tree_filename)
  bd_nltt_filename <- paste0(c(bd_base, "_nltts.csv"), collapse = "") # nolint unusued variable indeed
  # bd_tree_filename <- paste0(c(bd_base, ".tree"), collapse = "") # nolint commented code should be deleted
  bd_log_filename <- paste0(c(bd_base, ".log"), collapse = "") # nolint unusued variable indeed

  mbd_base <- tools::file_path_sans_ext(bd_tree_filename)
  mbd_nltt_filename <- paste0(c(mbd_base, "_nltts.csv"), collapse = "") # nolint unusued variable indeed
  # tree_filename <- paste0(c(mbd_base, ".tree"), collapse = "") # nolint commented code should be deleted
  mbd_log_filename <- paste0(c(mbd_base, ".log"), collapse = "") # nolint unusued variable indeed

  # The true tree
  # * bd_tree <- ape::read.tree(file = bd_tree_filename)
  # * mbd_tree <- ape::read.tree(file = mbd_tree_filename)

  bd_sim <- get(load(
    bd_tree_filename
  ))
  bd_tree <- bd_sim$bd_tree

  mbd_sim <- get(load(
    mbd_tree_filename
  ))
  mbd_tree <- mbd_sim$tes # nolint unused variable indeed



  # The posterior
  warning(
    "'log_filename' does not exist, ",
    "creating it here to make the tests pass"
  )
  log_filename <- NULL; rm(log_filename)

  posterior <- tracerer::parse_beast_posterior(
    trees_filename,
    log_filename
  )

  # TODO: create the nLTT file
  # Perhaps already work
  if (1 == 2) {
    nltt_diff <- rep(NA, length(posterior$trees))
    for (i in 1:length(posterior$trees)) {
      tree <- bd_tree # TODO: use one of the two, the function creates one file
      nltt_diff[i] <- nLTT::nLTTstat(tree, posterior$trees[[i]]) # nolint nLTT used older coding standard
    }
    utils::write.csv(
      x = nltt_diff,
      file = file.path(folder_name, "nonsense_nltt.csv")
    )
  }
  # TODO: stub
  testit::assert(!"Do something proper") # nolint accepted idiom
  nltt_filename <- NULL
  nltt_filename
}
