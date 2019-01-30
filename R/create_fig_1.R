#' @title Create nllt figure
#' @description Create nllt figure
#' @inheritParams default_params_doc
#' @return the nltt plot
#' @author Giovanni Laudanno
#' @export
create_fig_1 <- function(
  project_folder_name
) {

  # ISSUE #138 STUB

  lambda <- 0.2
  mu <- 0.1
  nu <- 2
  q <- 0.1

  df <- data.frame(
   par_setting = c(1, 2),
   nltt = c(0.1, 0.2),
   gen_model = c("1", "2")
  )
  if (1) {
    # need to call these variables to avoid notes in check
    df2 <- df
    df2$nltt <- df$nltt
    df2$par_setting <- df$par_setting
    df2$gen_model <- df$gen_model
    par_setting <- nltt <- gen_model <- 0
    lambda + mu + q + nu +
      length(df2$par_setting) + length(df2$nltt)  + length(df2$gen_model) > 0
  }
  xlabels <- c("1", "2")
  pl <- ggplot2::ggplot(df) +
    ggplot2::geom_boxplot(ggplot2::aes(
      x = par_setting,
      y = nltt,
      color = gen_model
    )) +
    ggplot2::facet_grid(. ~ gen_model
    ) +
    ggplot2::xlab(bquote(
      "Parameter setting" ~ "(" ~ lambda ~ "," ~ mu ~ ","
      ~ nu ~ "," ~ q ~ ")"
    )) +
    ggplot2::ylab(bquote(
      nltt
    )) +
    ggplot2::theme(
      axis.line = ggplot2::element_line(
        colour = "darkblue",
        size = 1,
        linetype = "solid"
      )) +
    ggplot2::scale_x_discrete(labels = xlabels); pl
}
