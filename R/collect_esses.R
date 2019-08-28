#' @title Collect esses
#' @description Collect esses
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and esses
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
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
  folder <- get_data_paths(project_folder_name, full_names = FALSE) # nolint internal function
  paths <- file.path(project_folder_name, folder)

  # information needed
  traces_names <- c(
    "Sample",
    "posterior",
    "likelihood"
  )
  setting_string_names <- c(
    "tree",
    "best_or_gen"
  )
  esses <- data.frame(matrix(
    NA,
    ncol = 1 + length(setting_string_names) + 1
  ))
  colnames(esses) <- c(
    "folder",
    "ess_likelihood",
    setting_string_names
  )
  # Reading files and store data
  i <- 0
  for (p in seq_along(paths)) {
    parameters <- readRDS(file.path(paths[p], "parameters.RDa")) # nolint internal function
    burn_in_fraction <-
      parameters$pir_params$error_measure_params$burn_in_fraction
    # pirouette checks that all experiments' MCMCs are identical
    sample_interval <-
      parameters$pir_params$experiments[[1]]$inference_model$mcmc$store_every
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
      x3 <- x2[, colnames(x2) %in% traces_names]
      data_table <- data.frame(x3, row.names = NULL)
      if (is_twin == TRUE) {
        data_table$tree <- "twin"
      } else {
        data_table$tree <- "true"
      }
      if (is_best == TRUE) {
        info_function <- get_best_model
        data_table$best_or_gen <- "best"
      }
      if (is_generative == TRUE) {
        info_function <- get_generative_model
        data_table$best_or_gen <- "gen"
      }
      if (!all(traces_names %in% names(data_table))) {
        msg <- "Not all 'traces_names' are present in data frame. \n"
        for (traces_name in traces_names) {
          if (!traces_name %in% names(data_table)) {
            msg <- c(msg, paste0("'", traces_name, "' is absent. \n"))
          }
        }
        stop(paste0(msg, collapse = ""))
      }
      testit::assert(all(traces_names %in% names(data_table)))
      traces <- data.frame(apply(
        data.frame(data_table[, traces_names]),
        MARGIN = 2,
        FUN = as.numeric
      ))
      # Remove burn-ins, burn_in_fraction obtained earlier
      clean_traces <- tracerer::remove_burn_ins(
        traces = traces,
        burn_in_fraction = burn_in_fraction
      )
      # Calculate the correct ESSes, sample_interval obtained earlier
      i <- i + 1
      esses[i, ]$ess_likelihood <- unlist(tracerer::calc_esses(
        clean_traces,
        sample_interval = sample_interval
      ))["likelihood"]
      for (par_name in setting_string_names) {
        par_value <- unique(unname(data_table[names(data_table) == par_name]))
        testit::assert(length(par_value) == 1)
        esses[i, ][names(esses) == par_name] <- par_value
      }
      esses[i, ]$folder <- folder[p]
    }
  }

  esses$tree <- as.factor(esses$tree)
  esses$best_or_gen <- as.factor(esses$best_or_gen)

  # Remove duplicates (no idea how they got in)
  esses <- unique(esses)

  # Order by true/twin then gen/best
  plyr::arrange(df = esses, folder, tree, plyr::desc(best_or_gen))
}
