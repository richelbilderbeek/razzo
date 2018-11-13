#' @title Collect esses
#' @description Collect esses
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and esses
#' @author Giovanni Laudanno
#' @export
raz_collect_esses <- function(
  filename
) {

  return() #STUB

  if (basename(filename) != "razzo_project") {
    stop("'folder_name' must end with 'razzo_project'")
  }

  data_folder <- file.path(
    filename,
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
        bd_mar <- utils::read.csv(file.path(m, "bd.log"))[-1]
        mbd_mar <- utils::read.csv(file.path(m, "mbd.log"))[-1]

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
  res2 <- data.frame(matrix(
    NA,
    ncol = len_par_mar / 2,
    nrow = 2 * df_length
  ))
  colnames(res2) <- mar_names
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
