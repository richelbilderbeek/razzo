#' For each parameter setting create a folder and a parameters file
#' @return Create folders for each parameter setting and saves each setting in a file.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
raz_create_parameters_files <- function(folder_name,
                                         lambda.interval = c(0.2, 0.2),
                                         mu.interval = c(0.15, 0.15),
                                         nu.interval = seq(from = 1, to = 2.5, by = 0.5),
                                         q.interval = seq(from = 0.10, to = 0.20, by = 0.05),
                                         seed.interval = 1:1000,
                                         soc,
                                         age,
                                         cond
) {

  setwd(folder_name)

  lambda.interval <- unique(lambda.interval)
  mu.interval <- unique(mu.interval)
  nu.interval <- unique(nu.interval)
  q.interval <- unique(q.interval)

  Lpars <- length(lambda.interval) *
    length(mu.interval) *
    length(nu.interval) *
    length(q.interval)

  dir.create(file.path("razzo_project"), showWarnings = FALSE)
  parameters_filenames <- rep(NA, Lpars)
  i <- 1
  for (lambda in lambda.interval) {
    for (mu in mu.interval) {
      for (nu in nu.interval) {
        for (q in q.interval) {
          parsettings_name <- paste0(lambda, "-", mu, "-", nu, "-", q)
          dir.create(file.path("razzo_project/",
                               parsettings_name),
                     showWarnings = FALSE)
          for (seed in seed.interval) {
            seedfolder <- paste0("razzo_project/",
                                 parsettings_name, "/",
                                 seed)
            dir.create(file.path(seedfolder),
                       showWarnings = FALSE)

            parameters <- c(lambda = lambda,
                            mu = mu,
                            nu = nu,
                            q = q,
                            seed = seed,
                            cond = cond,
                            age = age,
                            soc = soc)
            parameters_filenames[i] <- paste0(seedfolder, "/parameters.csv")
            write.csv(parameters, file = parameters_filenames[i])
            i <- i + 1
          }
        }
      }
    }
  }
  return(parameters_filenames)
}

#' Create all parameters files in a folder.
#' In that folder, one folder is created per parameter file.
#' In each subfolder, a file 'parameters.csv' is created
#' @inheritParams default_params_doc
#' @return full paths of the files created
#' @author Richel J.C. Bilderbeek
#' @export
#'
# raz_create_parameters_files <- function(folder_name) {
#
#   # * folder_name (in our case, it's called 'data')
#   #    * 1
#   #      * parameters.csv
#   #    * 2
#   #      * parameters.csv
#   # Etcetera
#
#   # TODO: Create one sub-folder per parameter set
#   sub_folder_name <- "1"
#   local_path <- file.path(folder_name, sub_folder_name)
#   path <- file.path(local_path, "parameters.csv")
#
#   # TODO: Create the parameters file here
#
#   # Return the path to the file
#   path
# }
