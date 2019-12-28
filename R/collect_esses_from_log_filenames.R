#' Collect ESSes from log filenames
#' @export
collect_esses_from_log_filenames <- function(parameters, files_log) {
  burn_in_fraction <-
    parameters$pir_params$error_measure_params$burn_in_fraction
  # pirouette checks that all experiments' MCMCs are identical
  sample_interval <-
    parameters$pir_params$experiments[[1]]$inference_model$mcmc$store_every

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
      data_table$best_or_gen <- "best"
    }
    if (is_generative == TRUE) {
      data_table$best_or_gen <- "gen"
    }
    razzo::check_traces_in_df(traces_names = traces_names, df = data_table)
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

