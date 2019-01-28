#' Create figure 2 to visualize how
#' @inheritParams default_params_doc
#' @return the name of the file the plot is saved to
#' @author Richel J.C. Bilderbeek
#' @export
create_fig_2_file <- function(
  project_folder_name = getwd()
) {
  check_project_folder_name(project_folder_name)

  # create figure
  fig_2 <- create_fig_2(project_folder_name) # nolint internal function

  # save output
  results_folder <- get_results_path(project_folder_name) # nolint
  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)
  fig_2_filename <- file.path(
    results_folder,
    "figure_2.png"
  )
  ggplot2::ggsave(
    filename = fig_2_filename,
    plot = fig_2
  )
  fig_2_filename
}
