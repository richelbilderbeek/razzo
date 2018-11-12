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
  nu_interval <- c(1.0, 1.0) # Testing
  q_interval <- c(0.10, 0.10) # Testing
  seed_interval <- 1:2
  crown_age <- 15
  sequence_length <- 100 # Testing
  sample_interval <- 1000
  chain_length <- 3000 # Testing
  sub_chain_length <- 1000
  clock_model_interval <- raz_clock_models() # nolint internal function
  site_model_interval <- raz_site_models() # nolint internal function

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
            seed_folder <- file.path(
              project_folder_name,
              data_folder_name,
              parsettings_name,
              seed
            )
            dir.create(file.path(seed_folder),
                           showWarnings = FALSE)
            for (clock_model in clock_model_interval) {
              for (site_model in site_model_interval) {
                model_folder <- file.path(
                  seed_folder,
                  paste0(clock_model, "-", site_model)
                )
                dir.create(file.path(model_folder),
                           showWarnings = FALSE)

                parameters <- raz_create_params(
                  lambda = lambda,
                  mu = mu,
                  nu = nu,
                  q = q,
                  seed = seed,
                  crown_age = crown_age,
                  sequence_length = sequence_length,
                  sample_interval = sample_interval,
                  chain_length = chain_length,
                  sub_chain_length = sub_chain_length,
                  clock_model = clock_model,
                  site_model = site_model
                )

                parameters_filenames[i] <- file.path(
                  model_folder,
                  "parameters.csv"
                )
                utils::write.csv(parameters, file = parameters_filenames[i])
                i <- i + 1
              }
            }
          }
        }
      }
    }
  }
  parameters_filenames
}
