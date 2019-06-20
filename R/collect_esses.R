#' @title Collect esses
#' @description Collect esses
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and esses
#' @author Giovanni Laudanno
#' @export
collect_esses <- function(
  project_folder_name = get_razzo_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint

  # retrieve information from files
  paths <- get_data_paths(project_folder_name) # nolint internal function
  best_esses <- gen_esses <- data.frame()
  for (p in seq_along(paths)) {
    parameters <- open_parameters_file(file.path(paths[p], "parameters.RDa")) # nolint internal function
    pars <- parameters$mbd_params
    files_log <- list.files(paths[p], pattern = "*.log")
    if (length(files_log) == 0) {
      stop(
        "No .log files found at path '", paths[p], "' \n",
        "Maybe the razzo experiment is not run yet? \n"
      )
    }
    for (f in seq_along(files_log)) {
      is_twin <- grepl(files_log[f], pattern = "twin")
      is_best <- grepl(files_log[f], pattern = "best")
      is_generative <- grepl(files_log[f], pattern = "gen")
      x <- utils::read.table(
        file.path(paths[p], files_log[f]),
        row.names = NULL
      )
      x1 <- x[1, ]
      x2 <- x[-1, ]
      colnames(x2) <- levels(droplevels(
        unlist(
          x1,
          use.names = FALSE
        )
      ))
      esses <- data.frame(x2, row.names = NULL)
      if (is_twin == TRUE) {
        esses$gen_model <- "bd"
      } else {
        esses$gen_model <- "mbd"
      }
      if (is_best == TRUE) {
        esses$site_model <-
          get_best_model(paths[p])[[esses$gen_model[1]]]$site_model
        esses$clock_model <-
          get_best_model(paths[p])[[esses$gen_model[1]]]$clock_model
        esses$tree_prior <-
          get_best_model(paths[p])[[esses$gen_model[1]]]$tree_prior
        esses <- as.data.frame(esses, row.names = NULL)
        best_esses <- suppressWarnings(plyr::rbind.fill(
          best_esses,
          cbind(pars, esses)
        ))
        best_esses <- as.data.frame(best_esses, row.names = NULL)
      }
      if (is_generative == TRUE) {
        esses$site_model <-
          get_generative_model(paths[p])[[esses$gen_model[1]]]$site_model
        esses$clock_model <-
          get_generative_model(paths[p])[[esses$gen_model[1]]]$clock_model
        esses$tree_prior <-
          get_generative_model(paths[p])[[esses$gen_model[1]]]$tree_prior
        esses <- as.data.frame(esses, row.names = NULL)
        gen_esses <- suppressWarnings(plyr::rbind.fill(
          gen_esses,
          cbind(pars, esses)
        ))
        gen_esses <- as.data.frame(gen_esses, row.names = NULL)
      }
    }
  }
  best_esses$best_or_gen <- "best"
  gen_esses$best_or_gen <- "gen"
  all_esses <- plyr::rbind.fill(
    best_esses,
    gen_esses
  )
  all_esses$gen_model <- as.factor(all_esses$gen_model)
  all_esses$site_model <- as.factor(all_esses$site_model)
  all_esses$clock_model <- as.factor(all_esses$clock_model)
  all_esses$tree_prior <- as.factor(all_esses$tree_prior)
  colnames(all_esses)[which(colnames(all_esses) == "likelihood")] <-
    "ess_likelihood"
  all_esses <- as.data.frame(all_esses, row.names = NULL)
  all_esses
}
