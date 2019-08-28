#' @title Collect number of taxa
#' @description Collect number of taxa
#' @inheritParams default_params_doc
#' @return a dataframe with folder and taxa number for each phylogeny
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
collect_n_taxa <- function(
  project_folder_name = get_razzo_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint

  ##### Satisfy R CMD check #####

  # retrieve information from files
  folder <- get_data_paths(project_folder_name, full_names = FALSE) # nolint internal function
  paths <- file.path(project_folder_name, folder)

  n_taxa <- rep(-1, length(paths))
  for (p in seq_along(paths)) {
    newick_file <- list.files(paths[p], pattern = "^(mbd\\.tree|pbd\\.newick)$")
    testit::assert(length(newick_file) == 1)
    tree <- ape::read.tree(file.path(paths[p], newick_file))
    n_taxa[p] <- length(tree$tip.label)
  }
  out <- data.frame(
    folder = folder,
    n_taxa = as.numeric(n_taxa)
  )
  plyr::arrange(df = out, folder)
}
