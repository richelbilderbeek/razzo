#' Create figure 1
#' @inheritParams default_params_doc
#' @param model_type It can be either "gen" (stands for "generative") or "best"
#'  (stands for "best candidate").
#' @return Figure 1 as a ggplot2 plot
#' @author Giovanni Laudanno
#' @export
create_fig_1 <- function(
  project_folder_name = get_razzo_path("razzo_project"),
  model_type = "best"
) {
  if (!(model_type %in% c("best", "gen"))) {
    stop("'model_type' must be either 'best' or 'gen'.")
  }
  check_project_folder_name(project_folder_name)
  df0 <- collect_nltt_stats(project_folder_name)

  ##### Satisfy R CMD check #####
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable
  error_value <- NULL; rm(error_value) # nolint, fixes warning: no visible binding for global variable
  inference_model <- NULL; rm(inference_model) # nolint, fixes warning: no visible binding for global variable
  quantile <- NULL; rm(quantile) # nolint, fixes warning: no visible binding for global variable
  ..y.. <- NULL; rm(..y..) # nolint, fixes warning: no visible binding for global variable
  model_setting <- NULL; rm(model_setting) # nolint, fixes warning: no visible binding for global variable
  tree_and_model <- NULL; rm(tree_and_model) # nolint, fixes warning: no visible binding for global variable
  median <- NULL; rm(median) # nolint, fixes warning: no visible binding for global variable
  label_parsed <- NULL; rm(label_parsed) # nolint, fixes warning: no visible binding for global variable
  ..density.. <- NULL; rm(..density..) # nolint, fixes warning: no visible binding for global variable

  ##### Data Wrangling #####
  df1 <- df0[df0$best_or_gen == model_type, ]
  df1$tree <- plyr::revalue(
    df1$gen_model,
    c("mbd" = "true", "bd" = "twin"), warn_missing = TRUE
  )
  df1$gen_model <- NULL
  df1$best_or_gen <- NULL
  names(df1) <- gsub(names(df1), pattern = "nltt_", replacement = "error_")
  n_errors <- sum(grepl(names(df1), pattern = "error_"))
  col_order <- c(
    mbd::get_param_names(),
    "seed",
    "crown_age",
    "cond",
    "tree",
    "site_model",
    "clock_model",
    "tree_prior",
    paste0("error_", 1:n_errors)
  )
  df2 <- df1[, col_order]

  # Convert to long form
  first_col_index <- which(names(df2) == "error_1")
  df_long <- tidyr::gather(
    df2, "error_index", "error_value", first_col_index:ncol(df2)
  )

  # Convert factor values to human-readable strings
  df_long$site_model <- plyr::revalue(
    df_long$site_model, c("JC69" = "JC", "TN93" = "TN"), warn_missing = FALSE)
  df_long$clock_model <- plyr::revalue(
    df_long$clock_model,
    c("strict" = "Strict", "relaxed_log_normal" = "RLN"), warn_missing = FALSE
  )
  df_long$tree_prior <- plyr::revalue(
    df_long$tree_prior,
    c(
      "yule" = "Yule",
      "birth_death" = "BD",
      "coalescent_bayesian_skyline" = "CBS",
      "coalescent_constant_population" = "CCP",
      "coalescent_exp_population" = "CEP"
    ),
    warn_missing = FALSE
  )

  # Add model_setting, the combination of all inference models
  df_long$model_setting <- interaction(
    df_long$site_model,
    df_long$clock_model,
    df_long$tree_prior,
    sep = ", "
  )
  df_long <- df_long[order(df_long$tree), ]
  df_long$model_setting <-
    factor(df_long$model_setting, levels = unique(df_long$model_setting))
  rownames(df_long) <- mapply(1:nrow(df_long), FUN = toString)

  ##### Theme #####
  label_size <- 13
  label_face <- "italic"
  title_size <- 18
  title_face <- "bold"
  ticks_size <- 12
  ticks_face <- "plain"
  ticks_color <- "black"
  theme_title <- ggplot2::element_text(
    hjust = 0.5, face = title_face, size = title_size
  )
  theme_major_label <- ggplot2::element_text(
    face = label_face, size = label_size
  )
  theme_minor_label <- ggplot2::element_text(
    face = ticks_face, color = ticks_color, size = ticks_size
  )
  theme <- ggplot2::theme(
    plot.title = theme_title,
    axis.title.x = theme_major_label,
    axis.title.y = theme_major_label,
    legend.title = theme_major_label,
    axis.text.x = theme_minor_label,
    axis.text.y = theme_minor_label,
    legend.text = theme_minor_label,
    strip.text.x = ggplot2::element_text(size = 12)
  )

  ##### Plot title #####
  plot_title <- "Inference errors in parameter space"

  ##### Legend labels #####
  get_first <- function(x) utils::head(x, n = 1)
  # True tree
  true_label <- NULL
  true_model <- get_first(
    df_long$model_setting[df_long$tree == "true"]
  )
  if (length(true_model)) {
    true_label <- paste("True:", true_model)
  }
  # Twin tree
  twin_label <- NULL
  twin_model <- get_first(
    df_long$model_setting[df_long$tree == "twin"]
  )
  if (length(twin_model)) {
    twin_label <- paste("Twin:", twin_model)
  }
  # Collect all labels. Absent models have NULL labels and are thus ignored
  tree_labels <- c(
    "true" = true_label,
    "twin" = twin_label
  )

  ##### Facet Labels
  # mu
  mu_values <- unique(df_long$mu)
  mu_labels <- paste0("mu==", mu_values) # what you show
  names(mu_labels) <- mu_values # what's in the df
  # nu
  nu_values <- unique(df_long$nu)
  nu_labels <- paste0("nu==", nu_values) # what you show
  names(nu_labels) <- nu_values # what's in the df
  # q
  q_values <- unique(df_long$q)
  q_labels <- paste0("q==", q_values) # what you show
  names(q_labels) <- q_values # what's in the df

  ##### Fill and line colors #####
  tree_colors <- c(
    "true" = "orangered",
    "twin" = "dodgerblue4"
  )

  ##### Plot #####
  bindwidth <- 0.05
  fig_1 <- ggplot2::ggplot(
    data = df_long,
    ggplot2::aes(
      x = error_value,
      color = tree,
      fill = tree
    )
  ) +
    ggplot2::geom_histogram(
      data = df_long,
      ggplot2::aes(y = bindwidth * ..density..),
      binwidth = bindwidth,
      alpha = 0.25,
      position = "identity"
    ) +
    ggplot2::facet_grid(
      mu + nu ~ q,
      labeller = ggplot2::labeller(
        mu = ggplot2::as_labeller(mu_labels, ggplot2::label_parsed),
        nu = ggplot2::as_labeller(nu_labels, ggplot2::label_parsed),
        q = ggplot2::as_labeller(q_labels, ggplot2::label_parsed)
      )
    ) +
    ggplot2::scale_color_manual(
      values = tree_colors,
      labels = tree_labels
    ) +
    ggplot2::scale_fill_manual(
      values = tree_colors,
      labels = tree_labels
    ) +
    ggplot2::labs(
      x = "Error",
      y = "Density",
      fill = "Tree and Model",
      color = "Tree and Model",
      title = plot_title
    ) + ggplot2::theme_bw() + theme
  fig_1
}
