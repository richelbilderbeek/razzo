#' @title Save nllt figure
#' @description Save nllt figure
#' @inheritParams default_params_doc
#' @return the file name of the plot
#' @author Giovanni Laudanno
#' @export
create_fig_1_file <- function(
  project_folder_name = getwd()
) {

  # create figure 1
  fig_1 <- create_fig_1(project_folder_name) # nolint internal function

  # save output
  results_folder <- get_results_path(project_folder_name) # nolint
  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)
  fig_1_filename <- file.path(
    results_folder,
    "figure_1.png"
  )

  ggplot2::ggsave(filename = fig_1_filename,
                  plot = fig_1
  )

  # return file name
  fig_1_filename
}
