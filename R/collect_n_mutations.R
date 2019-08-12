#' @title Collect number of mutations
#' @description Collect number of mutations
#' @inheritParams default_params_doc
#' @return a dataframe with folder and number of mutations
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
collect_n_mutations <- function(
  project_folder_name = get_razzo_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint

  # Paths to the folder, each folder holds a razzo experiment
  relative_paths <- get_data_paths(project_folder_name, full_names = FALSE) # nolint internal function
  paths <- file.path(project_folder_name, relative_paths)

  tree <- folder <- n_mutations <- rep(NA, 2 * length(paths))
  i <- 1
  for (p in seq_along(paths)) {
    files <- list.files(paths[p])
    parameters <- open_parameters_file(file.path(paths[p], "parameters.RDa")) # nolint internal function
    root_sequence <- parameters$pir_params$alignment_params$root_sequence
    target_files <- files[grepl(pattern = "*.fasta", x = files)]
    testit::assert(length(target_files) == 2)
    if (
      !("mbd.fasta" %in% files) |
      !("mbd_twin.fasta" %in% files)
    ) {
      stop(
        "No fasta files found at path '", paths[p], "' \n",
        "Maybe the razzo experiment is not run yet? \n"
      )
    }
    for (f in seq_along(target_files)) {
      alignment <- ape::read.FASTA(file.path(paths[p], target_files[f]))
      n_mutations[i] <- pirouette::count_n_mutations(
        alignment = alignment,
        root_sequence = root_sequence
      )
      folder[i] <- relative_paths[p]
      if (grepl(pattern = "twin", x = target_files[f])) {
        tree[i] <- "twin"
      } else {
        tree[i] <- "true"
      }
      i <- i + 1
    }
  }
  out <- data.frame(
    folder = folder,
    n_mutations = as.numeric(n_mutations),
    tree = tree
  )
  plyr::arrange(df = out, folder)
}
