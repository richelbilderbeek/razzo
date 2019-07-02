#' @title Collect esses
#' @description Collect esses
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and esses
#' @author Giovanni Laudanno
#' @export
collect_esses <- function(
  project_folder_name = get_razzo_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint

  ##### Satisfy R CMD check #####
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable
  seed <- NULL; rm(seed) # nolint, fixes warning: no visible binding for global variable
  best_or_gen <- NULL; rm(best_or_gen) # nolint, fixes warning: no visible binding for global variable

  # retrieve information from files
  paths <- get_data_paths(project_folder_name) # nolint internal function
  best_esses <- gen_esses <- data.frame()
  for (p in seq_along(paths)) {
    parameters <- open_parameters_file(file.path(paths[p], "parameters.RDa")) # nolint internal function
    pars <- parameters$mbd_params
    files_log <- list.files(paths[p], pattern = "*.log")
    if (length(files_log) == 0) {
      stop(
        "No .log files found at path '", paths[p], "' \n",
        "Maybe the razzo experiment is not run yet? \n"
      )
    }
    for (f in seq_along(files_log)) {
      is_twin <- grepl(files_log[f], pattern = "twin")
      is_best <- grepl(files_log[f], pattern = "best")
      is_generative <- grepl(files_log[f], pattern = "gen")
      x <- utils::read.table(
        file.path(paths[p], files_log[f]),
        row.names = NULL
      )
      x1 <- x[1, ]
      x2 <- x[-1, ]
      colnames(x2) <- levels(droplevels(
        unlist(
          x1,
          use.names = FALSE
        )
      ))
      esses <- data.frame(x2, row.names = NULL)
      if (is_twin == TRUE) {
        esses$gen_model <- "bd"
      } else {
        esses$gen_model <- "mbd"
      }
      if (is_best == TRUE) {
        esses$site_model <-
          get_best_model(paths[p])[[esses$gen_model[1]]]$site_model
        esses$clock_model <-
          get_best_model(paths[p])[[esses$gen_model[1]]]$clock_model
        esses$tree_prior <-
          get_best_model(paths[p])[[esses$gen_model[1]]]$tree_prior
        esses <- as.data.frame(esses, row.names = NULL)
        best_esses <- suppressWarnings(plyr::rbind.fill(
          best_esses,
          cbind(pars, esses)
        ))
        best_esses <- as.data.frame(best_esses, row.names = NULL)
      }
      if (is_generative == TRUE) {
        esses$site_model <-
          get_generative_model(paths[p])[[esses$gen_model[1]]]$site_model
        esses$clock_model <-
          get_generative_model(paths[p])[[esses$gen_model[1]]]$clock_model
        esses$tree_prior <-
          get_generative_model(paths[p])[[esses$gen_model[1]]]$tree_prior
        esses <- as.data.frame(esses, row.names = NULL)
        gen_esses <- suppressWarnings(plyr::rbind.fill(
          gen_esses,
          cbind(pars, esses)
        ))
        gen_esses <- as.data.frame(gen_esses, row.names = NULL)
      }
    }
  }
  best_esses$best_or_gen <- "best"
  gen_esses$best_or_gen <- "gen"
  all_esses <- plyr::rbind.fill(
    best_esses,
    gen_esses
  )
  all_esses <- as.data.frame(all_esses, row.names = NULL)
  all_esses$models <- interaction(
    all_esses$site_model,
    all_esses$clock_model,
    all_esses$tree_prior,
    all_esses$best_or_gen,
    all_esses$gen_model
  )
  all_esses$par_settings <- interaction(
    all_esses$lambda,
    all_esses$mu,
    all_esses$nu,
    all_esses$q,
    all_esses$seed,
    all_esses$crown_age,
    all_esses$cond
  )
  all_esses$settings <- interaction(
    all_esses$par_settings,
    all_esses$models
  )
  traces_names <- c(
    "Sample",
    "posterior",
    "likelihood",
    "prior",
    "treeLikelihood",
    "TreeHeight",
    "YuleModel",
    "birthRate"
  )
  setting_numeric_names <- c(
    "lambda",
    "mu",
    "nu",
    "q",
    "seed",
    "crown_age",
    "cond"
  )
  setting_string_names <- c(
    "gen_model",
    "site_model",
    "clock_model",
    "tree_prior",
    "best_or_gen"
  )
  # initialize dataframe components
  matrix_string <- data.frame(matrix(
    NA,
    ncol = length(setting_string_names),
    nrow = length(all_esses$settings)
  ))
  colnames(matrix_string) <- setting_string_names
  matrix_numeric <- data.frame(matrix(
    NA,
    ncol = length(setting_numeric_names),
    nrow = length(all_esses$settings)
  ))
  colnames(matrix_numeric) <- setting_numeric_names
  ess_likelihood <- rep(NA, length(all_esses$settings))
  for (i in seq_along(all_esses$settings)) {
    setting <- all_esses$settings[i]
    sub_set <- all_esses[all_esses$settings == setting, ]
    traces <- data.frame(apply(data.frame(sub_set[, traces_names]), MARGIN = 2, FUN = as.numeric))
    ess_likelihood[i] <- unlist(tracerer::calc_esses(traces, sample_interval = 1e6))["likelihood"]
    matrix_numeric[i, ] <- all_esses[
        all_esses$settings == setting,
        setting_numeric_names
        ][1, ]
    matrix_string[i, ] <- all_esses[
        all_esses$settings == setting,
        setting_string_names
        ][1, ]
  }
  esses <- cbind(
    matrix_numeric,
    ess_likelihood,
    matrix_string
  )
  esses$tree <- plyr::revalue(
    esses$gen_model,
    c("mbd" = "true", "bd" = "twin"),
    warn_missing = TRUE
  )
  esses$gen_model <- NULL

  esses$tree <- as.factor(esses$tree)
  esses$site_model <- as.factor(esses$site_model)
  esses$clock_model <- as.factor(esses$clock_model)
  esses$tree_prior <- as.factor(esses$tree_prior)

  # Remove duplicates (no idea how they got in)
  esses <- unique(esses)

  # Order by seed, then true/twin then gen/best
  plyr::arrange(esses, seed, tree, plyr::desc(best_or_gen))
}
