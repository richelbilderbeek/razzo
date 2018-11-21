#' @title Collect marginal loglikelihoods
#' @description Collect marginal loglikelihoods
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and marginal likelihoods
#' @author Giovanni Laudanno
#' @export
collect_marg_liks <- function(
  project_folder_name
) {

  # retrieve information from files
  data_paths <- get_data_paths(project_folder_name) # nolint internal function
  parameters <- open_parameters_file(file.path(data_paths[1], "parameters.csv")) # nolint internal function
  bd_mar <- utils::read.csv(file.path(data_paths[1], "bd_marg_lik.csv"))[-1]
  mbd_mar <- utils::read.csv(file.path(data_paths[1], "mbd_marg_lik.csv"))[-1]
  pars <- parameters[!grepl("model", names(parameters))]
  mars <- c(bd_mar, mbd_mar)
  par_names <- names(parameters)
  marg_names <- names(bd_mar)
  len_pars <- length(pars)
  len_mars <- length(mars)
  n_settings <- length(data_paths)

  # initialize dataframe components
  par_data <- data.frame(matrix(
    NA,
    ncol = len_pars,
    nrow = 2 * n_settings
  ))
  colnames(par_data) <- par_names[!grepl("model", par_names)]
  marg_data <- data.frame(matrix(
    NA,
    ncol = len_mars / 2,
    nrow = 2 * n_settings
  ))
  colnames(marg_data) <- marg_names
  gen_model <- clock_model <- site_model <- rep("blank", 2 * n_settings)

  # collect data
  i <- 1
  for (p in data_paths) {
    parameters <- open_parameters_file(file.path(p, "parameters.csv")) # nolint internal function
    bd_mar <- utils::read.csv(file.path(p, "bd_marg_lik.csv"))[-1]
    mbd_mar <- utils::read.csv(file.path(p, "mbd_marg_lik.csv"))[-1]
    par_num <- parameters[!grepl("model", names(parameters))]

    # save bd results
    par_data[i, ] <- data.frame(par_num)
    marg_data[i, ] <- unname(data.frame(bd_mar))
    site_model[i] <- levels(droplevels(parameters$site_model))
    clock_model[i] <- levels(droplevels(parameters$clock_model))
    gen_model[i] <- "bd"
    i <- i + 1

    # save mbd results
    par_data[i, ] <- data.frame(par_num)
    marg_data[i, ] <- unname(data.frame(mbd_mar))
    site_model[i] <- levels(droplevels(parameters$site_model))
    clock_model[i] <- levels(droplevels(parameters$clock_model))
    gen_model[i] <- "mbd"
    i <- i + 1
  }

  results <- cbind(par_data, gen_model, site_model, clock_model, marg_data)
  results
}
