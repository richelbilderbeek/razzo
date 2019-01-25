#' Create all parameter files  in
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @param experiment_type the type of experiment,
#'   can be either \code{test} or \code{full}
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_parameters_files <- function(
  project_folder_name = getwd(),
  experiment_type = "test"
) {
  testit::assert(experiment_type == "test" || experiment_type == "full")
  if (experiment_type == "test") {
    create_test_parameters_files(project_folder_name = project_folder_name) # nolint internal function
  } else {
    create_full_parameters_files(project_folder_name = project_folder_name) # nolint internal function
  }
}

#' Create the parameter files as used in the experiment
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_full_parameters_files <- function(
  project_folder_name = getwd()
) {
  # Just use the parameter combinations in the article
  lambda_interval <- c(0.2, 0.2)
  mu_interval <- c(0.15, 0.15)
  nu_interval <- c(1.0, 1.0) # Testing
  q_interval <- c(0.10, 0.10) # Testing
  seed_interval <- 1:2
  crown_age <- 15
  twinning_params <- pirouette::create_twinning_params()
  alignment_params <- pirouette::create_alignment_params(
    mutation_rate = 0.01
  )
  model_select_params <- pirouette::create_gen_model_select_param(
    alignment_params = alignment_params
  )
  inference_params <- pirouette::create_inference_params(
    mcmc = beautier::create_mcmc(chain_length = 5000, store_every = 1000)
  )

  lambda_interval <- unique(lambda_interval)
  mu_interval     <- unique(mu_interval)
  nu_interval     <- unique(nu_interval)
  q_interval      <- unique(q_interval)
  cond_interval   <- unique(cond_interval)

  l_pars <- length(lambda_interval) *
    length(mu_interval) *
    length(nu_interval) *
    length(q_interval) *
    length(cond_interval)

  data_folder_name <- "data"

  # Do not warn if the folder already exists
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
          for (cond in cond_interval) {
            parsettings_name <- paste0(lambda, "-", mu, "-", nu, "-", q)
            dir.create(
              file.path(
                project_folder_name,
                data_folder_name,
                parsettings_name
              ),
              showWarnings = FALSE
            )
            for (seed in seed_interval) {
              seed_folder <- file.path(
                project_folder_name,
                data_folder_name,
                parsettings_name,
                seed
              )
              dir.create(
                file.path(seed_folder),
                showWarnings = FALSE
              )
              mbd_params <- create_mbd_params(
                lambda = lambda,
                mu = mu,
                nu = nu,
                q = q,
                cond = cond,
                crown_age = crown_age,
                seed = seed
              )
              parameters <- create_razzo_params(
                mbd_params = mbd_params,
                twinning_params = twinning_params,
                alignment_params = alignment_params,
                inference_params = inference_params,
                model_select_params = model_select_params
              )
              parameters_filenames[i] <- file.path(
                seed_folder,
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
  parameters_filenames
}

#' Create all testing parameter files in
#'   \code{project_folder_name/data/[settings]/seed/[models]}
#' @inheritParams default_params_doc
#' @return Create folders for each parameter setting
#'   and saves each setting in a file within the corresponding folder.
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_test_parameters_files <- function(
  project_folder_name = getwd()
) {
  # Just use the parameter combinations in the article
  lambda_interval <- c(0.2, 0.2)
  mu_interval <- c(0.15, 0.15)
  nu_interval <- c(1.0, 1.0) # Testing
  q_interval <- c(0.10, 0.10) # Testing
  cond_interval <- 1
  seed_interval <- 1:2
  crown_age <- 15
  twinning_params <- pirouette::create_twinning_params()
  alignment_params <- pirouette::create_alignment_params(
    mutation_rate = 0.01
  )
  model_select_params <- pirouette::create_gen_model_select_param(
    alignment_params = alignment_params
  )
  inference_params <- pirouette::create_inference_params(
    mcmc = beautier::create_mcmc(chain_length = 5000, store_every = 1000)
  )

  lambda_interval <- unique(lambda_interval)
  mu_interval     <- unique(mu_interval)
  nu_interval     <- unique(nu_interval)
  q_interval      <- unique(q_interval)
  cond_interval   <- unique(cond_interval)

  l_pars <- length(lambda_interval) *
    length(mu_interval) *
    length(nu_interval) *
    length(q_interval) *
    length(cond_interval)

  data_folder_name <- "data"

  # Do not warn if the folder already exists
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
          for (cond in cond_interval) {
            parsettings_name <- paste0(lambda, "-", mu, "-", nu, "-", q)
            dir.create(
              file.path(
                project_folder_name,
                data_folder_name,
                parsettings_name
              ),
              showWarnings = FALSE
            )
            for (seed in seed_interval) {
              seed_folder <- file.path(
                project_folder_name,
                data_folder_name,
                parsettings_name,
                seed
              )
              dir.create(
                file.path(seed_folder),
                showWarnings = FALSE
              )
              mbd_params <- create_mbd_params(
                lambda = lambda,
                mu = mu,
                nu = nu,
                q = q,
                cond = cond,
                crown_age = crown_age,
                seed = seed
              )
              parameters <- create_razzo_params(
                mbd_params = mbd_params,
                twinning_params = twinning_params,
                alignment_params = alignment_params,
                inference_params = inference_params,
                model_select_params = model_select_params
              )
              parameters_filenames[i] <- file.path(
                seed_folder,
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
  parameters_filenames
}
