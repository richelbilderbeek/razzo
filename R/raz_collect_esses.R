#' @title Collect esses
#' @description Collect esses
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and esses
#' @author Giovanni Laudanno
#' @export
raz_collect_esses <- function(
  project_folder_name
) {

  return() # STUB

  if (basename(project_folder_name) != "razzo_project") {
    stop("'folder_name' must end with 'razzo_project'")
  }

  data_folder <- file.path(
    project_folder_name,
    "data"
  )

  # retrieve information from files
  paths <- raz_get_settings_paths(project_folder_name) # nolint internal function
  parameters <- raz_open_parameters_file(file.path(paths[1], "parameters.csv")) # nolint internal function
  x <- file.path(paths[1], "mbd.log")
  y <- file.path(paths[1], "bd.log")
  x2 <- read.table(x)
  y2 <- read.table(y)

  # Remove burn-ins
  estimates <- tracerer::remove_burn_ins(
    x2,
    burn_in_fraction = 0.1
  )

  # Calculate the effective sample sizes of all parameter estimates
  esses <- tracerer::calc_esses(
    estimates,
    sample_interval = parameters$sample_interval
  )

  pars <- parameters[!grepl("model", names(parameters))]
  mars <- c(bd_mar, mbd_mar)
  par_names <- names(parameters)
  marg_names <- names(bd_mar)
  len_pars <- length(pars)
  len_mars <- length(mars)
  n_settings <- length(paths)

  par_names <- names(parameters)
  marg_names <- names(bd_mar)
  df_length <- seed_len * length(par_settings_folders)
  len_par_num <- length(par_num)
  len_par_mar <- length(par_mar)

  par_data <- data.frame(matrix(
    NA,
    ncol = len_par_num,
    nrow = 2 * df_length
  ))
  colnames(par_data) <- par_names[!grepl("model", par_names)]
  res2 <- data.frame(matrix(
    NA,
    ncol = len_par_mar / 2,
    nrow = 2 * df_length
  ))
  colnames(res2) <- marg_names
  gen_model <- clock_model <- site_model <- rep("blank", 2 * df_length)

  i <- 1
  for (p in par_settings_folders) {
    seed_folders <- file.path(p, list.files(p))
    for (s in seed_folders) {
      model_folders <- file.path(s, list.files(s))
      for (m in model_folders) {
        gen_model[i] <- i
        clock_model[i] <- i
        site_model[i] <- i

        i <- i + 1
      }
    }
  }
}
