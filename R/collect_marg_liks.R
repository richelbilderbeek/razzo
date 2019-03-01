#' @title Collect marginal loglikelihoods
#' @description Collect marginal loglikelihoods
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and marginal likelihoods
#' @author Giovanni Laudanno
#' @export
collect_marg_liks <- function(
  project_folder_name = get_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint

  # retrieve information from files
  data_paths <- get_data_paths(project_folder_name) # nolint internal function
  parameters <- open_parameters_file(file.path(data_paths[1], "parameters.RDa")) # nolint internal function

  data_filename <- "mbd_marg_lik.csv"
  twin_data_filename <- pirouette::to_twin_filename(data_filename)

  bd_data <- utils::read.csv(file.path(data_paths[1], twin_data_filename))[-1]
  mbd_data <- utils::read.csv(file.path(data_paths[1], data_filename))[-1]
  pars <- parameters$mbd_params
  is_string <- which(grepl(pattern = "name", x = names(mbd_data)))
  is_numeric <- (1:ncol(mbd_data))[-is_string]
  names_pars <- names(parameters$mbd_params)
  names_string <- names(mbd_data[, is_string])
  names_numeric <- names(mbd_data[, is_numeric])
  len_pars <- length(pars)
  len_string <- length(is_string)
  len_numeric <- length(is_numeric)
  n_settings <- length(data_paths)

  # initialize dataframe components
  max_experiments <- length(beautier::create_site_models()) *
    length(beautier::create_clock_models()) *
    length(beautier::create_tree_priors())
  matrix_pars <- data.frame(matrix(
    NA,
    ncol = len_pars,
    nrow = 2 * n_settings * max_experiments
  ))
  colnames(matrix_pars) <- names_pars
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
  gen_model <- rep("blank", 2 * n_settings * max_experiments)

  # collect data
  i <- 1
  for (p in data_paths) {
    parameters <- open_parameters_file(file.path(p, "parameters.RDa")) # nolint internal function
    bd_data <- utils::read.csv(file.path(p, twin_data_filename))[-1]
    mbd_data <- utils::read.csv(file.path(p, data_filename))[-1]
    pars <- parameters$mbd_params

    # save bd results
    i_span <- nrow(bd_data)
    i_interval <- i:(i + i_span - 1)
    matrix_pars[i_interval, ] <- pars
    matrix_string[i_interval, ] <- sapply(bd_data[, is_string], as.character)
    matrix_numeric[i_interval, ] <- bd_data[, is_numeric]
    gen_model[i_interval] <- "bd"
    i <- i + i_span

    # save mbd results
    i_span <- nrow(mbd_data)
    i_interval <- i:(i + i_span - 1)
    matrix_pars[i_interval, ] <- pars
    matrix_string[i_interval, ] <- sapply(mbd_data[, is_string], as.character)
    matrix_numeric[i_interval, ] <- mbd_data[, is_numeric]
    gen_model[i_interval] <- "mbd"
    i <- i + i_span
  }
  gen_model <- gen_model[1:(i - 1)]
  results <- cbind(
    matrix_pars[1:(i - 1), ],
    matrix_string[1:(i - 1), ],
    matrix_numeric[1:(i - 1), ],
    gen_model
  )
  names(results) <- gsub(
    x = names(results),
    pattern = "_name",
    replacement = ""
  )
  results$gen_model <- as.factor(results$gen_model)
  results$site_model <- as.factor(results$site_model)
  results$clock_model <- as.factor(results$clock_model)
  results$tree_prior <- as.factor(results$tree_prior)
  results
}
