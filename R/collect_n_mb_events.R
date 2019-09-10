#' Collect the number of multiple-born (MB) species
#' @inheritParams default_params_doc
#' @return a dataframe with folder and number of multiple-born (MB) species
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
collect_n_mb_events <- function(
  project_folder_name = get_razzo_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint razzo function

  # Obtain all the tree filenames, as stored in Newick format
  true_tree_filenames <- list.files(
    path = project_folder_name,
    pattern = "^mbd\\.tree$",
    full.names = TRUE,
    recursive = TRUE
  )
  n_trees <- length(true_tree_filenames)

  # Issue 279, Issue #279
  n_mb_events <- rep(NA, n_trees)
  for (i in seq_along(true_tree_filenames)) {
    tree <- ape::read.tree(true_tree_filenames[[i]])
    brts <- sort(ape::branching.times(tree), decreasing = T)
    n_mb_events[i] <- mbd::count_n_mb_events(brts)
  }

  # Issue 279, Issue #279
  # Should give:
  #   data/0.2-0.15-1-0.1/1
  #   data/0.2-0.15-1-0.1/2
  folder_names <- get_data_paths(project_folder_name, full_names = FALSE) # nolint internal function

  df <- data.frame(
    folder = folder_names,
    n_mb_events = n_mb_events,
    stringsAsFactors = FALSE
  )

  assertive::assert_all_are_whole_numbers(df$n_mb_events)

  df
}
