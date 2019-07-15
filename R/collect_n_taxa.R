#' @title Collect number of taxa
#' @description Collect number of taxa
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and taxa number for each phylogeny
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
collect_n_taxa <- function(
  project_folder_name = get_razzo_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint

  ##### Satisfy R CMD check #####
  seed <- NULL; rm(seed) # nolint, fixes warning: no visible binding for global variable

  # retrieve information from files
  paths <- get_data_paths(project_folder_name) # nolint internal function
  # initialize dataframe components
  n_settings <- 2 * length(paths)
  mbd_pars <- open_parameters_file(file.path(paths[1], "parameters.RDa"))$mbd_params # nolint internal function
  setting_numeric_names <- c(names(mbd_pars), "n_taxa")
  setting_string_names <- c("site_model", "clock_model", "tree")
  matrix_string <- data.frame(matrix(
    NA,
    ncol = length(setting_string_names),
    nrow = n_settings
  ))
  colnames(matrix_string) <- setting_string_names
  matrix_numeric <- data.frame(matrix(
    NA,
    ncol = length(setting_numeric_names),
    nrow = n_settings
  ))
  colnames(matrix_numeric) <- setting_numeric_names
  i <- 1
  for (p in seq_along(paths)) {
    parameters <- open_parameters_file(file.path(paths[p], "parameters.RDa")) # nolint internal function
    mbd_pars <- parameters$mbd_params
    site_model <- parameters$pir_params$alignment_params$site_model$name
    clock_model <- parameters$pir_params$alignment_params$clock_model$name
    files <- list.files(paths[p])
    files_tree <- files[sapply(
      files, function(x)
        any(grepl("tree", x)) &
        !any(grepl("trees", x))
    )]
    if (length(files_tree) == 0) {
      stop(
        "No tree files found at path '", paths[p], "' \n",
        "Maybe the razzo experiment is not run yet? \n"
      )
    }
    for (f in seq_along(files_tree)) {
      is_twin <- grepl(files_tree[f], pattern = "twin")
      tree <- ape::read.tree(file.path(paths[p], files_tree[f]))
      n_taxa <- length(tree$tip.label)
      if (is_twin) {
        tree_type <- "twin"
      } else {
        tree_type <- "true"
      }
      matrix_numeric[i, ] <- cbind(mbd_pars, n_taxa)
      matrix_string[i, ] <- c(site_model, clock_model, tree_type)
      i <- i + 1
    }
  }
  colnames(matrix_numeric) <- setting_numeric_names
  colnames(matrix_string) <- setting_string_names
  out <- cbind(matrix_numeric, matrix_string)

  out$tree <- as.factor(out$tree)
  out$site_model <- as.factor(out$site_model)
  out$clock_model <- as.factor(out$clock_model)

  # Order by seed, then true/twin
  out <- plyr::arrange(out, seed, tree)
  out
}
