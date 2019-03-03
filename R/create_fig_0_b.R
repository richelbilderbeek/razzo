#' Create old figure 1: the marginal logliks plot
#' @inheritParams default_params_doc
#' @return the marginal logliks plot
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_fig_0_b <- function(
  project_folder_name
) {
  check_project_folder_name(project_folder_name)

  df <- collect_marg_liks(project_folder_name)

  # Satisfy R CMD check
  marg_log_lik <- NULL; rm(marg_log_lik) # nolint, fixes warning: no visible binding for global variable
  gen_model <- NULL; rm(gen_model) # nolint, fixes warning: no visible binding for global variable

  pl <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = marg_log_lik, fill = gen_model)
  ) + ggplot2::geom_histogram() +
    ggplot2::ggtitle("Figure 2")
  pl
}
