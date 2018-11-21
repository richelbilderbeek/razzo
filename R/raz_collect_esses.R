#' @title Collect esses
#' @description Collect esses
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and esses
#' @author Giovanni Laudanno
#' @export
collect_esses <- function(
  project_folder_name
) {

  if (basename(project_folder_name) != "razzo_project") {
    stop("'project_folder_name' must end with 'razzo_project'")
  }

  data_folder <- file.path(
    project_folder_name,
    "data"
  )

  # retrieve information from files
  paths <- get_settings_paths(project_folder_name) # nolint internal function
  parameters <- open_parameters_file(file.path(paths[1], "parameters.csv")) # nolint internal function
  x <- utils::read.delim(file.path(paths[1], "mbd.log"))

  esses <- tracerer::calc_esses(
    traces = tracerer::remove_burn_ins(
      traces = x,
      burn_in_fraction = 0.1
    ),
    sample_interval = parameters$sample_interval
  )

  pars <- parameters[!grepl("model", names(parameters))]
  par_names <- names(parameters)
  esses_names <- paste0("ess_", names(esses))
  len_pars <- length(pars)
  len_esses <- length(esses)
  n_settings <- length(paths)

  # initialize dataframe components
  par_data <- data.frame(matrix(
    NA,
    ncol = len_pars,
    nrow = 2 * n_settings
  ))
  colnames(par_data) <- par_names[!grepl("model", par_names)]
  esses_data <- data.frame(matrix(
    NA,
    ncol = len_esses,
    nrow = 2 * n_settings
  ))
  colnames(esses_data) <- esses_names
  gen_model <- clock_model <- site_model <- rep("blank", 2 * n_settings)

  # collect data
  i <- 1
  for (p in paths) {
    parameters <- open_parameters_file(file.path(p, "parameters.csv")) # nolint internal function
    par_num <- parameters[!grepl("model", names(parameters))]

    # save bd results
    par_data[i, ] <- data.frame(par_num)
    site_model[i] <- levels(droplevels(parameters$site_model))
    clock_model[i] <- levels(droplevels(parameters$clock_model))
    gen_model[i] <- "bd"
    bd_log <- utils::read.delim(file.path(p, "bd.log"))
    esses_data[i, ] <- tracerer::calc_esses(
      traces = tracerer::remove_burn_ins(
        traces = bd_log,
        burn_in_fraction = 0.1
      ),
      sample_interval = parameters$sample_interval
    )
    i <- i + 1

    # save mbd results
    par_data[i, ] <- data.frame(par_num)
    site_model[i] <- levels(droplevels(parameters$site_model))
    clock_model[i] <- levels(droplevels(parameters$clock_model))
    gen_model[i] <- "mbd"
    mbd_log <- utils::read.delim(file.path(p, "mbd.log"))
    esses_data[i, ] <- tracerer::calc_esses(
      traces = tracerer::remove_burn_ins(
        traces = mbd_log,
        burn_in_fraction = 0.1
      ),
      sample_interval = parameters$sample_interval
    )
    i <- i + 1
  }

  results <- cbind(
    par_data,
    gen_model,
    site_model,
    clock_model,
    esses_data[, -ncol(esses_data)]
  )
  results
}
