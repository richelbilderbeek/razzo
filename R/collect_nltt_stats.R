#' @title Collect nltt statistics
#' @description Collect nltt statistics
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and nltt statistics
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
collect_nltt_stats <- function(
  project_folder_name = get_razzo_path("razzo_project")
) {
  razzo::check_project_folder_name(project_folder_name)

  ##### Satisfy R CMD check #####
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable
  best_or_gen <- NULL; rm(best_or_gen) # nolint, fixes warning: no visible binding for global variable
  folder <- NULL; rm(folder) # nolint, fixes warning: no visible binding for global variable


  # Paths to the folder, each folder holds a razzo experiment
  relative_paths <-
    razzo::get_data_paths(project_folder_name, full_names = FALSE)
  paths <- file.path(project_folder_name, relative_paths)

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
  beautier::check_file_exists(first_filename, "first_filename")
  matrix_folder <- data.frame(matrix(
    NA,
    ncol = 1,
    nrow = n_files
  ))
  colnames(matrix_folder) <- "folder"
  names_string <- c(
    "tree",
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
        matrix_string$tree[i] <- "twin"
      } else {
        matrix_string$tree[i] <- "true"
      }
      if (is_best == TRUE) {
        matrix_string$best_or_gen[i] <- "best"
      }
      if (is_generative == TRUE) {
        matrix_string$best_or_gen[i] <- "gen"
      }
      matrix_folder[i, ] <- relative_paths[p]
      i <- i + 1
    }
  }

  # combine results
  matrix_string$tree <- as.factor(matrix_string$tree)
  matrix_string$best_or_gen <- as.factor(matrix_string$best_or_gen)

  results <- cbind(
    matrix_folder,
    matrix_string,
    matrix_nltts
  )
  plyr::arrange(df = results, folder, tree, plyr::desc(best_or_gen))
}
