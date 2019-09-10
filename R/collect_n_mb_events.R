#' Collect the number of multiple-birth (MB) events
#' @inheritParams default_params_doc
#' @return a dataframe with folder and number of multiple-birth (MB) events
#' @author Richel J.C. Bilderbeek
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
  # STUB
  # These are just random values
  n_mb_events <- round(
    runif(n = n_trees, min = 0, max = 10)
  )

  # Issue 279, Issue #279
  # STUB
  # Should give:
  #   data/0.2-0.15-1-0.1/1
  #   data/0.2-0.15-1-0.1/2
  folder_names <- file.path("data", 1:n_trees)

  df <- data.frame(
    folder = folder_names,
    n_mb_events = n_mb_events,
    stringsAsFactors = FALSE
  )

  assertive::assert_all_are_whole_numbers(df$n_mb_events)

  df
}
