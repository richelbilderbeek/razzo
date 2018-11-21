#' @title Collect nltt statistics
#' @description Collect nltt statistics
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and nltt statistics
#' @author Giovanni Laudanno
#' @export
raz_collect_nltt_stats <- function(
  project_folder_name = getwd()
) {
  check_project_folder_name(project_folder_name) # nolint internal function

  # retrieve information from files
  paths <- raz_get_settings_paths(project_folder_name) # nolint internal function

  parameters <- raz_open_parameters_file(file.path(paths[1], "parameters.csv")) # nolint internal function
  len_nltt <- 0
  for (p in paths) {
    bd_nltt_filename <- file.path(p, "bd_nltts.csv")
    mbd_nltt_filename <- file.path(p, "mbd_nltts.csv")
    raz_check_file_exists(bd_nltt_filename) # nolint internal function
    raz_check_file_exists(mbd_nltt_filename) # nolint internal function
    bd_nltt <- utils::read.csv(bd_nltt_filename)[, 2]
    mbd_nltt <- utils::read.csv(mbd_nltt_filename)[, 2]
    len_nltt <- pmax(len_nltt, length(bd_nltt), length(mbd_nltt))
  }
  pars <- parameters[!grepl("model", names(parameters))]
  par_names <- names(parameters)
  nltt_names <- paste0("nltt_", (1:len_nltt))
  len_pars <- length(pars)
  n_settings <- length(paths)

  # initialize dataframe components
  par_data <- data.frame(matrix(
    NA,
    ncol = len_pars,
    nrow = 2 * n_settings
  ))
  colnames(par_data) <- par_names[!grepl("model", par_names)]
  nltt_data <- data.frame(matrix(
    NA,
    ncol = len_nltt,
    nrow = 2 * n_settings
  ))
  colnames(nltt_data) <- nltt_names
  gen_model <- clock_model <- site_model <- rep("blank", 2 * n_settings)

  # collect data
  i <- 1
  for (p in paths) {
    parameters <- raz_open_parameters_file(file.path(p, "parameters.csv")) # nolint internal function
    bd_nltt_filename <- file.path(p, "bd_nltts.csv")
    mbd_nltt_filename <- file.path(p, "mbd_nltts.csv")
    raz_check_file_exists(bd_nltt_filename) # nolint internal function
    raz_check_file_exists(mbd_nltt_filename) # nolint internal function
    bd_temp <- utils::read.csv(bd_nltt_filename)[, 2]
    mbd_temp <- utils::read.csv(mbd_nltt_filename)[, 2]
    mbd_nltt <- bd_nltt <- rep(NA, len_nltt)
    bd_nltt[1:length(bd_temp)] <- bd_temp
    mbd_nltt[1:length(mbd_temp)] <- mbd_temp
    par_num <- parameters[!grepl("model", names(parameters))]

    # save bd results
    par_data[i, ] <- data.frame(par_num)
    nltt_data[i, ] <- unlist(unname(data.frame(bd_nltt)))
    site_model[i] <- levels(droplevels(parameters$site_model))
    clock_model[i] <- levels(droplevels(parameters$clock_model))
    gen_model[i] <- "bd"
    i <- i + 1

    # # save mbd results
    par_data[i, ] <- data.frame(par_num)
    nltt_data[i, ] <- unlist(unname(data.frame(mbd_nltt)))
    site_model[i] <- levels(droplevels(parameters$site_model))
    clock_model[i] <- levels(droplevels(parameters$clock_model))
    gen_model[i] <- "mbd"
    i <- i + 1
  }

  results <- cbind(par_data, gen_model, site_model, clock_model, nltt_data)
  results
}
