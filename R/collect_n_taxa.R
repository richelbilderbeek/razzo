#' Collect the number of taxa for analyses.
#'
#' After a razzo experiment is run, this function extracts the number
#' of taxa from the resulting files. In practice, this number is extracted
#' from all files named \code{mbd.tree} or \code{pbd.newick}.
#' @description Collect number of taxa
#' @inheritParams default_params_doc
#' @return a dataframe with folder and taxa number for each phylogeny
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @examples
#' library(testthat)
#' library(raztr)
#'
#' df <- collect_n_taxa(
#'   project_folder_name = get_raztr_path("razzo_project")
#' )
#'
#' expect_true("folder" %in% names(df))
#' expect_true("n_taxa" %in% names(df))
#' expect_true(is.character(df$folder))
#' expect_true(is.numeric(df$n_taxa))
#' @export
collect_n_taxa <- function(
  project_folder_name = getwd()
) {
  razzo::check_project_folder_name(project_folder_name)

  ##### Satisfy R CMD check #####

  # retrieve information from files
  folder <- razzo::get_data_paths(project_folder_name, full_names = FALSE)
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
    n_taxa = as.numeric(n_taxa),
    stringsAsFactors = FALSE
  )
  plyr::arrange(df = out, folder)
}
