#' @title Collect nltt statistics
#' @description Collect nltt statistics
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and nltt statistics
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
collect_nltt_stats <- function(
  project_folder_name = get_razzo_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint

  # retrieve information from files
  paths <- get_data_paths(project_folder_name) # nolint internal function
  n_files <- 0
  max_len_nltts <- 0
  for (p in seq_along(paths)) {
    files_nltt <- list.files(paths[p], pattern = "nltt")
    n_files <- n_files + length(files_nltt)
    for (f in seq_along(files_nltt)) {
      len_nltts <- length(utils::read.csv(
        file.path(paths[p], files_nltt[f]),
        row.names = NULL
      )[, -1])
      max_len_nltts <- max(max_len_nltts, len_nltts)
    }
  }

  # define matrices to store data
  first_filename <- file.path(paths[1], "parameters.RDa")
  # Fails on '/tmp/RtmpitOdlW/razzo_project/data/0_twin.2-0.15-1-0.1/1/parameters.RDa' # nolint indeed a long path
  testit::assert(file.exists(first_filename))
  parameters <- open_parameters_file(first_filename) # nolint internal function
  pars <- parameters$mbd_params
  matrix_pars <- data.frame(matrix(
    NA,
    ncol = length(pars),
    nrow = n_files
  ))
  colnames(matrix_pars) <- names(pars)
  names_string <- c(
    "site_model",
    "clock_model",
    "tree_prior",
    "gen_model",
    "best_or_gen"
  )
  matrix_string <- data.frame(matrix(
    NA,
    ncol = length(names_string),
    nrow = n_files
  ))
  colnames(matrix_string) <- names_string
  matrix_nltts <- data.frame(matrix(
    NA,
    ncol = max_len_nltts,
    nrow = n_files
  ))
  colnames(matrix_nltts) <- paste0("nltt_", 1:max_len_nltts)

  # loop over all files
  i <- 1
  for (p in seq_along(paths)) {
    parameters <- open_parameters_file(file.path(paths[p], "parameters.RDa")) # nolint internal function
    pars <- parameters$mbd_params
    files_nltt <- list.files(paths[p], pattern = "nltt")
    for (f in seq_along(files_nltt)) {
      is_twin <- grepl(files_nltt[f], pattern = "twin")
      is_best <- grepl(files_nltt[f], pattern = "best")
      is_generative <- grepl(files_nltt[f], pattern = "gen")
      matrix_nltts[i, ] <- utils::read.csv(
        file.path(paths[p], files_nltt[f]),
        row.names = NULL
      )[, -1]
      if (is_twin == TRUE) {
        matrix_string$gen_model[i] <- "bd"
      } else {
        matrix_string$gen_model[i] <- "mbd"
      }
      if (is_best == TRUE) {
        matrix_string$site_model[i] <-
          get_best_model(paths[p])[[matrix_string$gen_model[i]]]$site_model
        matrix_string$clock_model[i] <-
          get_best_model(paths[p])[[matrix_string$gen_model[i]]]$clock_model
        matrix_string$tree_prior[i] <-
          get_best_model(paths[p])[[matrix_string$gen_model[i]]]$tree_prior
        matrix_string$best_or_gen[i] <- "best"
      }
      if (is_generative == TRUE) {
        matrix_string$site_model[i] <-
          get_generative_model(paths[p])[[matrix_string$gen_model[i]]]$site_model
        matrix_string$clock_model[i] <-
          get_generative_model(paths[p])[[matrix_string$gen_model[i]]]$clock_model
        matrix_string$tree_prior[i] <-
          get_generative_model(paths[p])[[matrix_string$gen_model[i]]]$tree_prior
        matrix_string$best_or_gen[i] <- "gen"
      }
      matrix_pars[i, ] <- pars
      i <- i + 1
    }
  }

  # combine results
  matrix_string$gen_model <- as.factor(matrix_string$gen_model)
  matrix_string$site_model <- as.factor(matrix_string$site_model)
  matrix_string$clock_model <- as.factor(matrix_string$clock_model)
  matrix_string$tree_prior <- as.factor(matrix_string$tree_prior)
  results <- cbind(
    matrix_pars,
    matrix_string,
    matrix_nltts
  )
  results
}
