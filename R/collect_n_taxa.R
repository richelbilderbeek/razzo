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

  # retrieve information from files
  folder <- get_data_paths(project_folder_name, full_names = FALSE) # nolint internal function
  paths <- file.path(project_folder_name, folder)

  # initialize dataframe components
  n_settings <- length(paths)
  n_taxa <- rep(NA, n_settings)
  setting_string_names <- c("site_model", "clock_model")
  matrix_string <- data.frame(matrix(
    NA,
    ncol = length(setting_string_names),
    nrow = n_settings
  ))
  colnames(matrix_string) <- setting_string_names
  for (p in seq_along(paths)) {
    parameters <- open_parameters_file(file.path(paths[p], "parameters.RDa")) # nolint internal function
    site_model <- parameters$pir_params$alignment_params$site_model$name
    clock_model <- parameters$pir_params$alignment_params$clock_model$name
    files <- list.files(paths[p])
    if (!("mbd.tree" %in% files)) {
      stop(
        "No tree files found at path '", paths[p], "' \n",
        "Maybe the razzo experiment is not run yet? \n"
      )
    }
    tree <- ape::read.tree(file.path(paths[p], "mbd.tree"))
    n_taxa[p] <- length(tree$tip.label)
    matrix_string[p, ] <- c(site_model, clock_model)
  }
  colnames(matrix_string) <- setting_string_names
  out <- cbind(
    folder,
    n_taxa,
    matrix_string
  )

  out$site_model <- as.factor(out$site_model)
  out$clock_model <- as.factor(out$clock_model)

  out
}
