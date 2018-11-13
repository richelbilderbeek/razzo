#' @title Collect marginal loglikelihoods
#' @description Collect marginal loglikelihoods
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and marginal likelihoods
#' @author Giovanni Laudanno
#' @aliases raz_collect_mar_log_liks
#' @export
raz_collect_marg_log_liks <- function(
  project_folder_name
) {

  if (basename(project_folder_name) != "razzo_project") {
    stop("'folder_name' must end with 'razzo_project'")
  }

  data_folder <- file.path(
    project_folder_name,
    "data"
  )

  par_settings_folders <- file.path(data_folder, list.files(data_folder))
  for (p in par_settings_folders[1]) {
    seed_folders <- file.path(p, list.files(p))
    for (s in seed_folders[1]) {
      seed_len <- length(list.files(s))
      model_folders <- file.path(s, list.files(s))
      for (m in model_folders[1]) {
        parameters <- raz_open_parameters_file(file.path(m, "parameters.csv")) # nolint internal function
        bd_mar <- utils::read.csv(file.path(m, "bd_mar_log_lik.csv"))[-1]
        mbd_mar <- utils::read.csv(file.path(m, "mbd_mar_log_lik.csv"))[-1]

        par_num <- parameters[!grepl("model", names(parameters))]
        par_mar <- c(bd_mar, mbd_mar)
      }
    }
  }
  par_names <- names(parameters)
  mar_names <- names(bd_mar)
  df_length <- seed_len * length(par_settings_folders)
  len_par_num <- length(par_num)
  len_par_mar <- length(par_mar)

  par_data <- data.frame(matrix(
    NA,
    ncol = len_par_num,
    nrow = 2 * df_length
  ))
  colnames(par_data) <- par_names[!grepl("model", par_names)]
  mar_data <- data.frame(matrix(
    NA,
    ncol = len_par_mar / 2,
    nrow = 2 * df_length
  ))
  colnames(mar_data) <- mar_names
  gen_model <- clock_model <- site_model <- rep("blank", 2 * df_length)

  i <- 1
  for (p in par_settings_folders) {
    seed_folders <- file.path(p, list.files(p))
    for (s in seed_folders) {
      model_folders <- file.path(s, list.files(s))
      for (m in model_folders) {
        parameters <- raz_open_parameters_file(file.path(m, "parameters.csv")) # nolint internal function
        bd_mar <- utils::read.csv(file.path(m, "bd_mar_log_lik.csv"))[-1]
        mbd_mar <- utils::read.csv(file.path(m, "mbd_mar_log_lik.csv"))[-1]
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
    }
  }
  results <- cbind(par_data, gen_model, site_model, clock_model, mar_data)
  results
}
