#' Create all parameter files  in
#'   \code{project_folder_name/data/[settings]/seed}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
raz_create_parameters_files <- function(
  project_folder_name = getwd()
) {
  # Just use the parameter combinations in the article
  lambda_interval <- c(0.2, 0.2)
  mu_interval <- c(0.15, 0.15)
  nu_interval <- seq(from = 1, to = 2.5, by = 0.5)
  q_interval <- seq(from = 0.10, to = 0.20, by = 0.05)
  seed_interval <- 1:2
  crown_age <- 15
  sequence_length <- 1000
  mbd_mutation_rate <- 1.0 / crown_age
  bd_mutation_rate <- 1.0 / crown_age
  sample_interval <- 1000
  chain_length <- 10000
  sub_chain_length <- 1000

  lambda_interval <- unique(lambda_interval)
  mu_interval     <- unique(mu_interval)
  nu_interval     <- unique(nu_interval)
  q_interval      <- unique(q_interval)

  l_pars <- length(lambda_interval) *
           length(mu_interval) *
           length(nu_interval) *
           length(q_interval)

  data_folder_name <- "data"

  dir.create(
    file.path(project_folder_name, data_folder_name),
    showWarnings = TRUE
  )
  testit::assert(dir.exists(file.path(project_folder_name, data_folder_name)))
  parameters_filenames <- rep(NA, l_pars)
  i <- 1
  for (lambda in lambda_interval) {
    for (mu in mu_interval) {
      for (nu in nu_interval) {
        for (q in q_interval) {
          parsettings_name <- paste0(lambda, "-", mu, "-", nu, "-", q)
          dir.create(file.path(project_folder_name,
                               data_folder_name,
                               parsettings_name),
                     showWarnings = FALSE)
          for (seed in seed_interval) {
            seedfolder <- file.path(project_folder_name,
                                    data_folder_name,
                                    parsettings_name,
                                    seed)
            dir.create(file.path(seedfolder),
                       showWarnings = FALSE)

            # TODO: Use 'raz_create_params',
            # like 'parameters <- raz_create_params(...)'
            parameters <- raz_create_params(
              lambda = lambda,
              mu = mu,
              nu = nu,
              q = q,
              seed = seed,
              crown_age = crown_age,
              sequence_length = sequence_length,
              mbd_mutation_rate = mbd_mutation_rate,
              bd_mutation_rate = bd_mutation_rate,
              sample_interval = sample_interval,
              chain_length = chain_length,
              sub_chain_length = sub_chain_length
            )

            parameters_filenames[i] <- file.path(seedfolder, "parameters.csv")
            utils::write.csv(parameters, file = parameters_filenames[i])
            i <- i + 1
          }
        }
      }
    }
  }
  parameters_filenames
}
