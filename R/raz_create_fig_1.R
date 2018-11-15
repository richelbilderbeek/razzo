#' @title Create nllt figure
#' @description Create nllt figure
#' @inheritParams default_params_doc
#' @return the nltt plot
#' @author Giovanni Laudanno
#' @export
raz_create_fig_1 <- function(
  project_folder_name
) {

  par_setting <- nltt <- gen_model <- NULL

  df0 <- raz_collect_nltt_stats(project_folder_name)
  df0$par_setting <- interaction(
    df0$lambda,
    df0$mu,
    df0$nu,
    df0$q,
    df0$site_model,
    df0$clock_model
  )

  df <- tidyr::gather(
    df0,
    "i",
    "nltt",
    (1:ncol(df0))[grepl("nltt", names(df0))]
  )
  xlabels <- unique(paste0(
    df0$lambda,
    "\n",
    df$mu,
    "\n",
    df$nu,
    "\n",
    df$q,
    "\n",
    df$site_model,
    "\n",
    df$clock_model
  ))

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
      ~ nu ~ "," ~ q ~ "," ~ site ~ "," ~ clock ~ ")"
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
