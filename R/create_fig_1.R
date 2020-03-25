#' Create figure 1
#' @inheritParams default_params_doc
#' @return Figure 1 as a ggplot2 plot
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @export
create_fig_1 <- function(
  project_folder_name = getwd(),
  bins = 15
) {
  razzo::check_project_folder_name(project_folder_name)
  df0 <- razzo::collect_nltt_stats(project_folder_name)
  ##### Satisfy R CMD check #####
  tree <- NULL; rm(tree) # nolint, fixes warning: no visible binding for global variable
  lambda <- NULL; rm(lambda) # nolint, fixes warning: no visible binding for global variable
  mu <- NULL; rm(mu) # nolint, fixes warning: no visible binding for global variable
  nu <- NULL; rm(nu) # nolint, fixes warning: no visible binding for global variable
  error_value <- NULL; rm(error_value) # nolint, fixes warning: no visible binding for global variable
  inference_model <- NULL; rm(inference_model) # nolint, fixes warning: no visible binding for global variable
  quantile <- NULL; rm(quantile) # nolint, fixes warning: no visible binding for global variable
  ..y.. <- NULL; rm(..y..) # nolint, fixes warning: no visible binding for global variable
  tree_and_model <- NULL; rm(tree_and_model) # nolint, fixes warning: no visible binding for global variable
  median <- NULL; rm(median) # nolint, fixes warning: no visible binding for global variable
  label_parsed <- NULL; rm(label_parsed) # nolint, fixes warning: no visible binding for global variable
  ..density.. <- NULL; rm(..density..) # nolint, fixes warning: no visible binding for global variable
  ##### Data Wrangling #####
  df_errors <- df0
  df_errors$inference_model <- plyr::revalue(
    df_errors$best_or_gen,
    c("generative" = "GEN", "candidate" = "best"), warn_missing = FALSE
  )
  df_errors$best_or_gen <- NULL
  names(df_errors) <- gsub(
    names(df_errors), pattern = "nltt_", replacement = "error_"
  )
  n_errors <- sum(grepl(names(df_errors), pattern = "error_"))
  df_params <- razzo::collect_mbd_params(project_folder_name)
  df_merged <- merge(x = df_params, y = df_errors, by = "folder")
  col_order <- c(
    "folder",
    mbd::get_param_names(),
    "seed",
    "crown_age",
    "cond",
    "tree",
    "inference_model",
    paste0("error_", 1:n_errors)
  )
  df2 <- df_merged[, col_order]
  # Convert to long form
  first_col_index <- which(names(df2) == "error_1")
  df_long <- tidyr::gather(
    df2, "error_index", "error_value", first_col_index:ncol(df2)
  )
  # Add column tree_and_model, the combination of tree and model type
  df_long$tree_and_model <- interaction(
    df_long$tree,
    df_long$inference_model,
    sep = "_"
  )
  df_long <- df_long[order(df_long$tree), ]
  df_long$inference_model <-
    factor(df_long$inference_model, levels = unique(df_long$inference_model))
  rownames(df_long) <- sprintf("%.0f", seq_len(nrow(df_long)))
  ##### Theme #####
  label_size <- 13
  label_face <- "italic"
  title_size <- 18
  title_face <- "bold.italic"
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
    strip.text.x = ggplot2::element_text(size = 12),
    strip.placement = "outside"
  ) +
    ggplot2::theme_bw()
  ##### Legend labels #####
  tg_label <- paste("Generative, true.")
  wg_label <- paste("Generative, twin.")
  tb_label <- paste("Best, true.")
  wb_label <- paste("Best, twin.")
  tree_and_model_labels <- c(
    tg_label,
    wg_label,
    tb_label,
    wb_label
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
  # Line colors: must be darker than the fill color
  tree_and_model_line_colors <- c(
    "true_gen" = "#FF0000", # Red
    "twin_gen" = "#E77E22", # Orange
    "true_best" = "#0000FF", # Blue
    "twin_best" = "#229955"  # Green
  )
  # Fill colors: must be lighter than the colors at the edges
  tree_and_model_fill_colors <- c(
    "true_gen" = "#FF3333", # Red
    "twin_gen" = "#F99F55", # Orange
    "true_best" = "#3333FF", # Blue
    "twin_best" = "#559988"  # Green
  )
  ##### Only keep 95% of x axis values #####
  index <- trunc(0.95 * length(df_long$error_value)) # split by mu, nu, q?
  x_top <- sort(df_long$error_value)[index]
  ##### More aesthetic settings for the plots #####
  n_error <- length(unique(df_long$error_index))
  bindwidth <- x_top / sqrt(n_error)
  alpha <- 0.5
  bins <- 15
  # Collect the medians
  medians <- df_long %>%
    dplyr::group_by(lambda, mu, nu, q, tree_and_model) %>%
    dplyr::summarise(median = stats::median(error_value))
  medians$inference_model <- gsub(
    x = gsub(
      x = medians$tree_and_model,
      pattern = "true_",
      replacement = ""
    ),
    pattern = "twin_",
    replacement = ""
  )
  df_long$inference_model <- plyr::revalue(
    df_long$inference_model,
    c(
      "candidate" = "Best",
      "generative" = "Generative"
    ),
    warn_missing = FALSE
  )
  medians$inference_model <- plyr::revalue(
    medians$inference_model,
    c(
      "candidate" = "Best",
      "generative" = "Generative"
    ),
    warn_missing = FALSE
  )
  ##### Plot #####
  ii <- 1
  mus <- unique(df_long$mu)
  plots <- vector("list", length(mus))
  mts <- unique(df_long$inference_model) # model_types
  for (i_mt in seq_along(mts)) {
    df_mt <- df_long[df_long$inference_model == mts[i_mt], ]
    medians_mt <- medians[medians$inference_model == mts[i_mt], ]
    for (i_mu in seq_along(mus)) {
      df_mu <- df_mt[df_mt$mu == mus[i_mu], ]
      medians_mu <- medians_mt[medians_mt$mu == mus[i_mu], ]
      plots[[ii]] <-
        ggplot2::ggplot(
          data = df_mu,
          ggplot2::aes(
            x = error_value,
            color = tree_and_model,
            fill = tree_and_model
          )
        ) +
        ggplot2::geom_histogram(
          data = df_mu,
          ggplot2::aes(y = bindwidth * ..density..), # nolint the dots in ..density.. are not improper ways to separate words here
          bins = bins,
          alpha = alpha,
          position = "identity"
        ) +
        ggplot2::facet_wrap(
          nu ~ q,
          labeller = ggplot2::labeller(
            mu = ggplot2::as_labeller(mu_labels, ggplot2::label_parsed),
            nu = ggplot2::as_labeller(nu_labels, ggplot2::label_parsed),
            q = ggplot2::as_labeller(q_labels, ggplot2::label_parsed)
          ),
          strip.position = "top",
          scales = "free",
          ncol = length(q_values)
        ) +
        ggplot2::scale_color_manual(
          values = tree_and_model_line_colors,
          labels = tree_and_model_labels
        ) +
        ggplot2::scale_fill_manual(
          values = tree_and_model_fill_colors,
          labels = tree_and_model_labels
        ) +
        ggplot2::geom_vline(
          data = medians_mu,
          ggplot2::aes(
            xintercept = median,
            color = tree_and_model
          ),
          linetype = "dashed"
        ) +
        ggplot2::ggtitle(paste0(
          "Inference error distribution: lambda = ",
          unique(df_mu$lambda),
          ", mu = ",
          mus[i_mu],
          ", crown age = ",
          unique(df_mu$crown_age)
        )) +
        ggplot2::labs(
          x = "Error",
          y = "Density",
          fill = "Model and tree",
          color = "Model and tree"
        ) + theme
      ii <- ii + 1
    }
  }
  plots
}
