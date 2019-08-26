#' @title Collect all results
#' @description Collect all results
#' @inheritParams default_params_doc
#' @param include_all_nltt TRUE if you want ALL the nltt data (which might be
#'  a lot)
#' @return a dataframe that summarizes all the results
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
collect_results <- function(
  project_folder_name = get_razzo_path("razzo_project"),
  include_all_nltt = FALSE
) {
  ##### Satisfy R CMD check #####
  sd <- NULL; rm(sd) # nolint, fixes warning: no visible binding for global variable
  folder <- NULL; rm(folder) # nolint, fixes warning: no visible binding for global variable
  best_or_gen <- NULL; rm(best_or_gen) # nolint, fixes warning: no visible binding for global variable

  mbd_params <- TRUE
  n_taxa <- TRUE
  esses <- TRUE
  marg_liks <- FALSE
  nltt_stats <- TRUE
  n_mutations <- TRUE
  selected_stats <- c(
    mbd_params,
    n_taxa,
    esses,
    marg_liks,
    nltt_stats,
    n_mutations
  )
  fun_list <- c(
    "collect_mbd_params",
    "collect_n_taxa",
    "collect_esses",
    "collect_marg_liks",
    "collect_nltt_stats",
    "collect_n_mutations"
  )
  fun_list <- fun_list[selected_stats]
  df_names <- gsub(x = fun_list, pattern = "collect_", replacement = "")
  dfs <- vector("list", length(fun_list))
  names(dfs) <- df_names
  for (i in 1:length(dfs)) {
    dfs[[i]] <- get(fun_list[i])(project_folder_name)
  }
  nltt_data <-
    dfs$nltt_stats[grepl(x = names(dfs$nltt_stats), pattern = "nltt_")]
  nltt_means <- rowMeans(nltt_data)
  nltt_sd <- apply(X = nltt_data, MARGIN = 1, FUN = sd)
  if (include_all_nltt == FALSE) {
    dfs$nltt_stats <-
      dfs$nltt_stats[!grepl(x = names(dfs$nltt_stats), pattern = "nltt_")]
  }
  dfs$nltt_stats$nltt_means <- nltt_means
  dfs$nltt_stats$nltt_sd <- nltt_sd
  testit::assert(nrow(dfs$n_taxa) == nrow(dfs$mbd_params))
  df_small <- merge(x = dfs$mbd_params, y = dfs$n_taxa)
  testit::assert(nrow(df_small) == nrow(dfs$mbd_params))
  df_small <- merge(x = df_small, y = dfs$n_mutations)
  testit::assert(nrow(dfs$nltt_stats) == nrow(dfs$esses))
  df_big <- merge(x = dfs$esses, y = dfs$nltt_stats)
  testit::assert(nrow(df_big) == nrow(dfs$esses))
  df <- merge(x = df_small, y = df_big)
  testit::assert(nrow(df_big) == nrow(df))
  plyr::arrange(df = df, folder, best_or_gen)
}
