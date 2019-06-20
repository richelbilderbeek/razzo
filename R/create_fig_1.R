#' Create figure 1
#' @inheritParams default_params_doc
#' @return Figure 1 as a ggplot2 plot
#' @author Giovanni Laudanno
#' @export
create_fig_1 <- function(
  project_folder_name = get_razzo_path("razzo_project")
) {
  check_project_folder_name(project_folder_name)

  df0 <- collect_nltt_stats(project_folder_name)

  # ### \start: remove this! # nolint
  # how_many_nu <- 4 # nolint
  # how_many_q <- 3 # nolint
  # df00 <- df0 # nolint
  # roba <- NULL # nolint
  # for (add_nu in 0:(how_many_nu - 1)) {# nolint
  #   for (add_q in 0:(how_many_q - 1)) {# nolint
  #     proxy <- df0 # nolint
  #     proxy[, "nu"] <- df00[, "nu"] + add_nu # nolint
  #     proxy[, "q"] <- df00[, "q"] + add_q * 0.5 # nolint
  #     roba <- rbind(roba, proxy) # nolint
  #   } # nolint
  # } # nolint
  # df0 <- roba # nolint
  # ### \end: remove this! # nolint

  df1 <- df0
  df1$tree <- plyr::revalue(
    df0$gen_model,
    c("mbd" = "true", "bd" = "twin"), warn_missing = TRUE
  )
  df1$gen_model <- NULL
  df1$inference_model <- plyr::revalue(
    df0$best_or_gen,
    c("best" = "candidate", "gen" = "generative"), warn_missing = TRUE
  )
  df1$best_or_gen <- NULL
  names(df1) <- gsub(names(df1), pattern = "nltt_", replacement = "error_")
  n_errors <- sum(grepl(names(df1), pattern = "error_"))
  col_order <- c(
    mbd::get_param_names(),
    "seed",
    "crown_age",
    "cond",
    "tree",
    "inference_model",
    "site_model",
    "clock_model",
    "tree_prior",
    paste0("error_", 1:n_errors)
  )
  df2 <- df1[, col_order]
  df2$par_setting <- interaction(
    df1[
      c(
        mbd::get_param_names(),
        "crown_age",
        "cond"
      )
      ]
  )
  df3 <- df2[, c(
    "par_setting",
    "tree",
    "inference_model",
    "site_model",
    "clock_model",
    "tree_prior",
    paste0("error_", 1:n_errors)
  )]
  par_settings <- unique(df3$par_setting)
  n_par_settings <- length(par_settings)
  plots <- vector("list", n_par_settings)
  for (i in 1:n_par_settings) {
    par_setting <- par_settings[i]
    plots[[i]] <- pir_plot(
      pir_out = df3[df3$par_setting == par_setting,]
    ) +
      ggplot2::ggtitle(NULL) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::xlab(NULL) +
      ggplot2::ylab(NULL)
  }

  if (1) {
    # need to call these variables to avoid notes in check
    df <- df0
    lambda <- df$lambda
    mu <- df$mu
    nu <- df$nu
    q <- df$q
    nltt <- df$nltt
    rm(lambda, mu, nu, q, nltt, df)
  }
  n_plots <- length(plots)
  nCol <- floor(sqrt(n_plots))
  grid_function <- gridExtra::grid.arrange
  out <- do.call(grid_function, c(plots, ncol = nCol))
  out
}
