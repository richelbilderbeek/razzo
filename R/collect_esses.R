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
  data_table_best <- data_table_gen <- data.frame()
  for (p in seq_along(paths)) {
    parameters <- open_parameters_file(file.path(paths[p], "parameters.RDa")) # nolint internal function
    pars <- parameters$mbd_params
    burn_in_fraction <-
      parameters$pir_params$error_measure_params$burn_in_fraction
    sample_interval <-
      parameters$pir_params$experiments[[1]]$inference_model$mcmc$store_every # pirouette checks that all experiments' MCMCs are identical
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
      data_table <- data.frame(x2, row.names = NULL)
      data_table$burn_in_fraction <- burn_in_fraction
      data_table$sample_interval <- sample_interval
      if (is_twin == TRUE) {
        data_table$gen_model <- "bd"
      } else {
        data_table$gen_model <- "mbd"
      }
      if (is_best == TRUE) {
        data_table$site_model <-
          get_best_model(paths[p])[[data_table$gen_model[1]]]$site_model
        data_table$clock_model <-
          get_best_model(paths[p])[[data_table$gen_model[1]]]$clock_model
        data_table$tree_prior <-
          get_best_model(paths[p])[[data_table$gen_model[1]]]$tree_prior
        data_table <- as.data.frame(data_table, row.names = NULL)
        data_table_best <- suppressWarnings(plyr::rbind.fill(
          data_table_best,
          cbind(pars, data_table)
        ))
        data_table_best <- as.data.frame(data_table_best, row.names = NULL)
      }
      if (is_generative == TRUE) {
        data_table$site_model <-
          get_generative_model(paths[p])[[data_table$gen_model[1]]]$site_model
        data_table$clock_model <-
          get_generative_model(paths[p])[[data_table$gen_model[1]]]$clock_model
        data_table$tree_prior <-
          get_generative_model(paths[p])[[data_table$gen_model[1]]]$tree_prior
        data_table <- as.data.frame(data_table, row.names = NULL)
        data_table_gen <- suppressWarnings(plyr::rbind.fill(
          data_table_gen,
          cbind(pars, data_table)
        ))
        data_table_gen <- as.data.frame(data_table_gen, row.names = NULL)
      }
    }
  }
  data_table_best$best_or_gen <- "best"
  data_table_gen$best_or_gen <- "gen"
  data_table_all <- plyr::rbind.fill(
    data_table_best,
    data_table_gen
  )
  data_table_all <- as.data.frame(data_table_all, row.names = NULL)
  data_table_all$models <- interaction(
    data_table_all$site_model,
    data_table_all$clock_model,
    data_table_all$tree_prior,
    data_table_all$best_or_gen,
    data_table_all$gen_model,
    data_table_all$burn_in_fraction,
    data_table_all$sample_interval
  )
  data_table_all$par_settings <- interaction(
    data_table_all$lambda,
    data_table_all$mu,
    data_table_all$nu,
    data_table_all$q,
    data_table_all$seed,
    data_table_all$crown_age,
    data_table_all$cond
  )
  data_table_all$settings <- interaction(
    data_table_all$par_settings,
    data_table_all$models
  )
  # We only use the posterior to estimate the ESS
  traces_names <- c(
    "Sample",
    "posterior",
    "likelihood"
  #  "prior",
  #  "treeLikelihood",
  #  "TreeHeight",
  #  "YuleModel",
  #  "birthRate"
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
    nrow = length(data_table_all$settings)
  ))
  colnames(matrix_string) <- setting_string_names
  matrix_numeric <- data.frame(matrix(
    NA,
    ncol = length(setting_numeric_names),
    nrow = length(data_table_all$settings)
  ))
  colnames(matrix_numeric) <- setting_numeric_names
  ess_likelihood <- rep(NA, length(data_table_all$settings))
  for (i in seq_along(data_table_all$settings)) {
    setting <- data_table_all$settings[i]
    sub_set <- data_table_all[data_table_all$settings == setting, ]
    if (!all(traces_names %in% names(sub_set))) {
      msg <- "Not all 'traces_names' are present in data frame. \n"
      for (traces_name in traces_names) {
        if (!traces_name %in% names(sub_set)) {
          msg <- c(msg, paste0("'", traces_name, "' is absent. \n"))
        }
      }
      stop(paste0(msg, collapse = ""))
    }
    testit::assert(all(traces_names %in% names(sub_set)))
    traces <- data.frame(apply(
      data.frame(sub_set[, traces_names]),
      MARGIN = 2,
      FUN = as.numeric
    ))
    # Remove burn-ins, burn_in_fraction obtained earlier
    testit::assert(length(unique(sub_set$burn_in_fraction)) == 1)
    clean_traces <- tracerer::remove_burn_ins(
      traces = traces,
      burn_in_fraction = sub_set$burn_in_fraction[1]
    )
    # Calculate the correct ESSes, sample_interval obtained earlier
    testit::assert(length(unique(sub_set$sample_interval)) == 1)
    ess_likelihood[i] <- unlist(tracerer::calc_esses(
      clean_traces,
      sample_interval = sub_set$sample_interval[1]
    ))["likelihood"]
    matrix_numeric[i, ] <- data_table_all[
        data_table_all$settings == setting,
        setting_numeric_names
        ][1, ]
    matrix_string[i, ] <- data_table_all[
        data_table_all$settings == setting,
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
