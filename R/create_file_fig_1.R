#' @title Save nllt figure
#' @description Save nllt figure
#' @inheritParams default_params_doc
#' @return the file name of the plot
#' @author Giovanni Laudanno
#' @aliases create_file_fig_1 create_fig_1_file
#' @export create_file_fig_1 create_fig_1_file
create_file_fig_1 <- create_fig_1_file <- function(
  project_folder_name = getwd()
) {

  fig_1 <- razzo::create_fig_1(project_folder_name)

  # save output
  results_folder <- razzo::get_results_path(project_folder_name)
  fig_width <- 10
  fig_height <- 10

  # No warning if folder already exists
  dir.create(results_folder, showWarnings = FALSE)

  if (is.list(fig_1)) {
    fig_1_filename <- c()
    for (i in seq_along(fig_1)) {
      fig_1_filename[i] <- file.path(
        results_folder,
        paste0("figure_1", letters[i], ".png")
      )
      ggplot2::ggsave(
        filename = fig_1_filename[i],
        plot = fig_1[[i]],
        width = fig_width,
        height = fig_height
      )
    }
  } else {
    fig_1_filename <- file.path(
      results_folder,
      "figure_1.png"
    )
    ggplot2::ggsave(
      filename = fig_1_filename,
      plot = fig_1,
      width = fig_width,
      height = fig_height
    )
  }

  # return file name
  fig_1_filename
}
