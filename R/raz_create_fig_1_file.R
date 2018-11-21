#' @title Save nllt figure
#' @description Save nllt figure
#' @inheritParams default_params_doc
#' @return the file name of the plot
#' @author Giovanni Laudanno
#' @export
raz_create_fig_1_file <- function(
  project_folder_name = getwd()
) {
  check_project_folder_name(project_folder_name) # nolint internal function

  # create figure 1
  fig_1 <- raz_create_fig_1(project_folder_name) # nolint internal function

  # save output
  results_folder <- file.path(
    project_folder_name,
    "results"
  )
  if (!dir.exists(results_folder)) {
    dir.create(results_folder)
  }
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
