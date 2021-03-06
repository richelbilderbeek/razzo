#' @title Collect marginal loglikelihoods
#' @description Collect marginal loglikelihoods
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and marginal likelihoods
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @aliases collect_evidences collect_marg_liks
#' @export collect_evidences collect_marg_liks
collect_evidences <- collect_marg_liks <- function(
  project_folder_name = getwd()
) {
  razzo::check_project_folder_name(project_folder_name)

  # retrieve information from files
  data_folder <- razzo::get_data_paths(project_folder_name, full_names = FALSE)
  data_paths <- file.path(project_folder_name, data_folder)

  # Take the first filename that ends with '_marg_lik.csv',
  # as it can be 'mbd_marg_lik.csv' and 'pbd_marg_lik.csv'
  data_filename <- "mbd_marg_lik.csv"
  data_filename <- list.files(path = data_paths, pattern = "_marg_lik\\.csv")[1]
  testit::assert(!is.na(data_filename))

  twin_data_filename <- pirouette::to_twin_filename(data_filename)

  bd_data <- utils::read.csv(file.path(data_paths[1], twin_data_filename))[-1]
  mbd_data <- utils::read.csv(file.path(data_paths[1], data_filename))[-1]
  is_string <- which(grepl(pattern = "name", x = names(mbd_data)))
  is_numeric <- (seq_len(ncol(mbd_data)))[-is_string]
  names_string <- names(mbd_data[, is_string])
  names_numeric <- names(mbd_data[, is_numeric])
  len_string <- length(is_string)
  len_numeric <- length(is_numeric)
  n_settings <- length(data_paths)

  # initialize dataframe components
  max_experiments <- length(beautier::create_site_models()) *
    length(beautier::create_clock_models()) *
    length(beautier::create_tree_priors())
  matrix_string <- data.frame(matrix(
    NA,
    ncol = len_string,
    nrow = 2 * n_settings * max_experiments
  ))
  colnames(matrix_string) <- names_string
  matrix_numeric <- data.frame(matrix(
    NA,
    ncol = len_numeric,
    nrow = 2 * n_settings * max_experiments
  ))
  colnames(matrix_numeric) <- names_numeric
  folder <- tree <- rep("blank", 2 * n_settings * max_experiments)

  # collect data
  i <- 1
  for (p in seq_along(data_paths)) {
    mbd_filename <- file.path(data_paths[p], data_filename)
    if (!file.exists(mbd_filename)) next

    bd_filename <- file.path(data_paths[p], twin_data_filename)
    if (!file.exists(bd_filename)) next

    bd_data <- utils::read.csv(bd_filename)[-1]
    mbd_data <- utils::read.csv(mbd_filename)[-1]

    # save bd results
    i_span <- nrow(bd_data)
    i_interval <- i:(i + i_span - 1)
    matrix_string[i_interval, ] <- sapply(bd_data[, is_string], as.character)
    matrix_numeric[i_interval, ] <- bd_data[, is_numeric]
    tree[i_interval] <- "twin"
    folder[i_interval] <- data_folder[p]
    i <- i + i_span

    # save mbd results
    i_span <- nrow(mbd_data)
    i_interval <- i:(i + i_span - 1)
    matrix_string[i_interval, ] <- sapply(mbd_data[, is_string], as.character)
    matrix_numeric[i_interval, ] <- mbd_data[, is_numeric]
    tree[i_interval] <- "true"
    folder[i_interval] <- data_folder[p]
    i <- i + i_span
  }
  tree <- tree[1:(i - 1)]
  folder <- folder[1:(i - 1)]
  results <- cbind(
    folder,
    matrix_string[1:(i - 1), ],
    tree,
    matrix_numeric[1:(i - 1), ]
  )
  names(results) <- gsub(
    x = names(results),
    pattern = "_name",
    replacement = ""
  )
  results$tree <- as.factor(results$tree)
  results$site_model <- as.factor(results$site_model)
  results$clock_model <- as.factor(results$clock_model)
  results$tree_prior <- as.factor(results$tree_prior)
  results
}
