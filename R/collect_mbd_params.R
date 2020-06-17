#' @title Collect mbd parameters
#' @description Collect mbd parameters
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and folder
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @export
collect_mbd_params <- function(
  project_folder_name = getwd()
) {
  razzo::check_project_folder_name(project_folder_name) # nolint

  ##### Satisfy R CMD check #####
  seed <- NULL; rm(seed) # nolint, fixes warning: no visible binding for global variable

  # retrieve information from files
  folder <- razzo::get_data_paths(project_folder_name, full_names = FALSE) # nolint internal function
  paths <- file.path(project_folder_name, folder)

  # Can we load the data?
  params_summary_file <- file.path(
    project_folder_name,
    "results",
    "mbd_params.csv"
  )
  if (file.exists(params_summary_file)) {
    params_summary <- utils::read.csv(params_summary_file)[, -1]
  }
  n_files_params <- length(list.files(paths, pattern = "parameters"))
  if (file.exists(params_summary_file)) {
    params_summary <- utils::read.csv(params_summary_file)[, -1]
    if (nrow(params_summary) == n_files_params) {
      return(params_summary)
    }
  }

  # initialize dataframe components
  n_settings <- length(paths)
  mbd_pars <- razzo::open_parameters_file(
    parameters_filename = file.path(paths[1], "parameters.RDa")
  )$mbd_params # nolint internal function
  setting_numeric_names <- names(mbd_pars)

  matrix_numeric <- data.frame(matrix(
    NA,
    ncol = length(setting_numeric_names),
    nrow = n_settings
  ))
  colnames(matrix_numeric) <- setting_numeric_names
  for (p in seq_along(paths)) {
    parameters <- razzo::open_parameters_file(file.path(paths[p], "parameters.RDa")) # nolint internal function
    mbd_pars <- parameters$mbd_params
    matrix_numeric[p, ] <- mbd_pars
  }
  out <- cbind(folder, matrix_numeric)
  out <- plyr::arrange(df = out, folder)
  save(out, file = params_summary_file)
  utils::write.csv(x = out, file = params_summary_file)
  out
}
