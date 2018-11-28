#' Create figure 2
#' @inheritParams default_params_doc
#' @return Figure 2 as a ggplot2 plot
#' @author Richel J.C. Bilderbeek
#' @export
create_fig_2 <- function(
  project_folder_name
) {
  check_project_folder_name(project_folder_name)

  marg_liks_filename <- file.path(
    project_folder_name, "results", "marg_liks.csv"
  )
  testit::assert(file.exists(marg_liks_filename))

  df <- read.csv(marg_liks_filename)[, -1]

  pl <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = marg_log_lik, fill = gen_model)
  ) + ggplot2::geom_histogram() +
    ggplot2::ggtitle("Figure 2")
  pl
}
