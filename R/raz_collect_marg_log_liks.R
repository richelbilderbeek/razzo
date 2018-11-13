#' @title Collect marginal loglikelihoods
#' @description Collect marginal loglikelihoods
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and marginal likelihoods
#' @author Giovanni Laudanno
#' @aliases raz_collect_mar_log_liks
#' @export
raz_collect_marg_log_liks <- function(
  filename
) {

  paths <- raz_get_settings_paths(filename)
  parameters <- raz_open_parameters_file(file.path(paths[1], "parameters.csv")) # nolint internal function
  bd_mar <- utils::read.csv(file.path(paths[1], "bd_mar_log_lik.csv"))[-1]
  mbd_mar <- utils::read.csv(file.path(paths[1], "mbd_mar_log_lik.csv"))[-1]

  pars <- parameters[!grepl("model", names(parameters))]
  mars <- c(bd_mar, mbd_mar)
  par_names <- names(parameters)
  mar_names <- names(bd_mar)
  len_pars <- length(pars)
  len_mars <- length(mars)
  n_settings <- length(paths)

  par_data <- data.frame(matrix(
    NA,
    ncol = len_pars,
    nrow = 2 * n_settings
  ))
  colnames(par_data) <- par_names[!grepl("model", par_names)]
  mar_data <- data.frame(matrix(
    NA,
    ncol = len_mars / 2,
    nrow = 2 * n_settings
  ))
  colnames(mar_data) <- mar_names
  gen_model <- clock_model <- site_model <- rep("blank", 2 * n_settings)

  i <- 1
  for (p in paths) {
    parameters <- raz_open_parameters_file(file.path(p, "parameters.csv")) # nolint internal function
    bd_mar <- utils::read.csv(file.path(p, "bd_mar_log_lik.csv"))[-1]
    mbd_mar <- utils::read.csv(file.path(p, "mbd_mar_log_lik.csv"))[-1]
    par_num <- parameters[!grepl("model", names(parameters))]

    # save bd results
    par_data[i, ] <- data.frame(par_num)
    mar_data[i, ] <- unname(data.frame(bd_mar))
    site_model[i] <- levels(droplevels(parameters$site_model))
    clock_model[i] <- levels(droplevels(parameters$clock_model))
    gen_model[i] <- "bd"
    i <- i + 1

    # save mbd results
    par_data[i, ] <- data.frame(par_num)
    mar_data[i, ] <- unname(data.frame(mbd_mar))
    site_model[i] <- levels(droplevels(parameters$site_model))
    clock_model[i] <- levels(droplevels(parameters$clock_model))
    gen_model[i] <- "mbd"

    i <- i + 1
  }

  results <- cbind(par_data, gen_model, site_model, clock_model, mar_data)
  results
}
