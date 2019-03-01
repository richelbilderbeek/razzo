#' @title Create nllt figure
#' @description Create nllt figure
#' @inheritParams default_params_doc
#' @return the nltt plot
#' @author Giovanni Laudanno
#' @export
create_fig_1 <- function(
  project_folder_name
) {
  check_project_folder_name(project_folder_name)

  df0 <- collect_nltt_stats(project_folder_name)
  df0$par_setting <- interaction(
    df0$lambda,
    df0$mu,
    df0$nu,
    df0$q,
    df0$site_model,
    df0$clock_model,
    df0$tree_prior
  )
  experiments <- c("gen", "best")
  real_names <- c("generative", "best candidate")
  pl <- vector("list", length(experiments))
  for (i in seq_along(experiments)) {
    df00 <- df0[df0$best_or_gen == experiments[i], ]
    df <- tidyr::gather(
      df00,
      "i",
      "nltt",
      (1:ncol(df00))[grepl("nltt", names(df00))]
    )
    if (1) {
      # need to call these variables to avoid notes in check
      df2 <- df
      lambda <- df2$lambda <- df$lambda
      mu <- df2$mu <- df$mu
      nu <- df2$nu <- df$nu
      q <- df2$q <- df$q
      df2$nltt <- df$nltt
      df2$par_setting <- df$par_setting
      df2$gen_model <- df$gen_model
      par_setting <- nltt <- gen_model <- 0

      lambda + mu + q + nu +
        length(df2$par_setting) + length(df2$nltt)  + length(df2$gen_model) > 0
      rm(lambda, mu, nu, q)
    }
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
      df$clock_model,
      "\n",
      df$tree_prior
    ))
    pl[[i]] <- ggplot2::ggplot(df) +
      ggplot2::geom_boxplot(ggplot2::aes(
        x = par_setting,
        y = nltt,
        color = gen_model
      )) +
      ggplot2::facet_grid(. ~ gen_model
      ) +
      ggplot2::xlab(bquote(
        "Parameter setting" ~ "(" ~ lambda ~ "," ~ mu ~ ","
        ~ nu ~ "," ~ q ~ "," ~ site ~ "," ~ clock ~ "," ~ prior ~ ")"
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
      ggplot2::scale_x_discrete(
        labels = xlabels
        ) +
      ggplot2::ggtitle(real_names[i])
  }
  gridExtra::grid.arrange(pl[[1]], pl[[2]], nrow = 1)
}
