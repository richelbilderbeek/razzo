#' @title Collect all data
#' @description Collect all data
#' @inheritParams default_params_doc
#' @param new_run If true will collect all data from scratch
#' @param coarse_grain choose how many digits precision for nltt
#' @return a dataframe with all data
#' @author Giovanni Laudanno
#' @export
collect_all_data <- function(
  project_folder_name = getwd(),
  new_run = FALSE,
  coarse_grain = 0
) {
  razzo::check_project_folder_name(project_folder_name) # nolint

  ##### Satisfy R CMD check #####
  folder <- NULL; rm(folder) # nolint, fixes warning: no visible binding for global variable
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable
  best_or_gen <- NULL; rm(best_or_gen) # nolint, fixes warning: no visible binding for global variable
  folder_tree <- NULL; rm(folder_tree) # nolint, fixes warning: no visible binding for global variable

  rf <- file.path(project_folder_name, "results")

  file <- file.path(rf, paste0("esses", ".csv"))
  if (!file.exists(file) | new_run) {
    razzo::create_esses_file(project_folder_name)
  }
  esses <- as.data.frame(utils::read.csv(file)[, -1])

  file <- file.path(rf, paste0("mbd_params", ".csv"))
  if (!file.exists(file) | new_run) {
    razzo::create_mbd_params_file(project_folder_name)
  }
  mbd_params <- as.data.frame(utils::read.csv(file)[, -1])

  file <- file.path(rf, paste0("n_mb_species", ".csv"))
  if (!file.exists(file) | new_run) {
    razzo::create_n_mb_species_file(project_folder_name)
  }
  n_mb_species <- as.data.frame(utils::read.csv(file)[, -1])

  file <- file.path(rf, paste0("n_mutations", ".csv"))
  if (!file.exists(file) | new_run) {
    razzo::create_n_mutations_file(project_folder_name)
  }
  n_mutations <- as.data.frame(utils::read.csv(file)[, -1])

  file <- file.path(rf, paste0("n_taxa", ".csv"))
  if (!file.exists(file) | new_run) {
    razzo::create_n_taxa_file(project_folder_name)
  }
  n_taxa <- as.data.frame(utils::read.csv(file)[, -1])

  file <- file.path(rf, paste0("nltt_stats", ".csv"))
  if (!file.exists(file) | new_run) {
    razzo::create_nltt_stats_file(project_folder_name)
  }
  nltt_stats <- as.data.frame(utils::read.csv(file)[, -1])

  # merge all
  df1 <- merge(esses, mbd_params, by = "folder"); dim(df1); colnames(df1)
  df2 <- merge(df1, n_mb_species, by = "folder"); dim(df2); colnames(df2)
  df2$folder_tree <- interaction(df2$folder, df2$tree)
  n_mutations$folder_tree <- interaction(n_mutations$folder, n_mutations$tree)

  if (length(setdiff(n_mutations$folder_tree, df2$folder_tree)) > 0) {
    x <- n_mutations
    x2 <-
      x[- (which(x$folder_tree %in% setdiff(x$folder_tree, df2$folder_tree))), ]
  } else {
    x2 <- n_mutations
  }

  # coarse grain?
  if (coarse_grain > 0) {
    if (!is.integer(coarse_grain)) {
      stop("'coarse_grain' must be an integer")
    }
    nltt_stats[grepl(x = colnames(nltt_stats), pattern = "nltt_")] <-
      DDD::roundn(
        nltt_stats[grepl(x = colnames(nltt_stats), pattern = "nltt_")],
        coarse_grain
      )
  }

  df3 <- merge(df2, droplevels(x2), by = c("folder", "tree"))
  df3$folder_tree.x <- NULL; df3$folder_tree.y <- NULL # nolint
  df4 <- merge(df3, n_taxa, by = c("folder"))
  df5 <- merge(df4, nltt_stats, by = c("folder", "tree", "best_or_gen"))
  errors <- df5[grepl(x = colnames(df5), pattern = "nltt_")]
  df5$median_nltt <- apply(errors, MARGIN = 1, "median")
  df5$mean_nltt <- apply(errors, MARGIN = 1, "mean")
  df5$variance_nltt <- apply(errors, MARGIN = 1, "var")
  df5$stdev_nltt <- apply(errors, MARGIN = 1, "sd")

  ### so far so good
  out <- plyr::arrange(df = df5, folder, tree, plyr::desc(best_or_gen))
  return(out)
}
