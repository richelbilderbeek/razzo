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

  ##### Data Wrangling #####
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

  mus <- unique(df2[, "mu"])
  plots_list <- df_list <- vector("list", length(mus))
  for (i in 1:length(mus)) {
    df_list[[i]] <- df2[df2[, "mu"] == mus[i], ]
  }
  for (i_mu in seq_along(df_list)) {

    ##### More Data Wrangling #####

    df <- df_list[[i_mu]][, -ncol(df_list[[i_mu]])]
    # Convert to long form
    first_col_index <- which(names(df) == "error_1")
    df_long <- tidyr::gather(
      df, "error_index", "error_value", first_col_index:ncol(df)
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

    # Add column tree_and_model, the combination of tree and model type
    df_long$tree_and_model <- interaction(
      df_long$tree,
      df_long$inference_model,
      sep = "_"
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
    df_long$inference_model <-
      factor(df_long$inference_model, levels = unique(df_long$inference_model))
    rownames(df_long) <- mapply(1:nrow(df_long), FUN = toString)

    ##### Theme #####
    label_size <- 13
    label_face <- "italic"
    title_size <- 18
    title_face <- "bold"
    subtitle_size <- 15
    subtitle_face <- "plain"
    ticks_size <- 12
    ticks_face <- "plain"
    ticks_color <- "black"
    theme_title <- ggplot2::element_text(
      hjust = 0.5, face = title_face, size = title_size
    )
    theme_subtitle <- ggplot2::element_text(
      hjust = 0.5, face = subtitle_face, size = subtitle_size
    )
    theme_major_label <- ggplot2::element_text(
      face = label_face, size = label_size
    )
    theme_minor_label <- ggplot2::element_text(
      face = ticks_face, color = ticks_color, size = ticks_size
    )
    theme <- ggplot2::theme(
      plot.title = theme_title,
      plot.subtitle = theme_subtitle,
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
    plot_subtitle <- bquote(mu == .(df_long$mu))

    ##### Legend labels #####

    get_first <- function(x) utils::head(x, n = 1)
    get_legend_labels <- function(df_long) {
      # True, Generative
      tg_label <- NULL
      tg_model <- get_first(
        df_long$model_setting[df_long$tree_and_model == "true_generative"]
      )
      if (length(tg_model)) {
        tg_label <- paste("Generative, true:", tg_model)
      }
      # Twin, Generative
      wg_label <- NULL
      wg_model <- get_first(
        df_long$model_setting[df_long$tree_and_model == "twin_generative"]
      )
      if (length(wg_model)) {
        wg_label <- paste("Generative, twin:", wg_model)
      }
      # True, Best
      tb_label <- NULL
      tb_model <- get_first(
        df_long$model_setting[df_long$tree_and_model == "true_candidate"]
      )
      if (length(tb_model)) {
        tb_label <- paste("Best, true:", tb_model)
      }
      # Twin, Best
      wb_label <- NULL
      wb_model <- get_first(
        df_long$model_setting[df_long$tree_and_model == "twin_candidate"]
      )
      if (length(wb_model)) {
        wb_label <- paste("Best, twin:", wb_model)
      }

      # Collect all labels. Absent models have NULL labels and are thus ignored
      tree_and_model_labels <- c(
        "true_generative" = tg_label,
        "twin_generative" = wg_label,
        "true_candidate" = tb_label,
        "twin_candidate" = wb_label
      )
    }

    ##### Fill and line colors #####

    # Line colors: must be darker than the fill color
    # Tree true has primary color, twin a lighter shade
    # Generative model is red, candidate blue
    tree_and_model_line_colors <- c(
      "true_generative" = "#FF0000", # Red
      "twin_generative" = "#FF8888", # Light red
      "true_candidate" = "#0000FF", # Blue
      "twin_candidate" = "#8888FF"  # Light blue
    )

    # Fill colors: must be lighter than the colors at the edges
    # Tree true has primary color, twin a lighter shade
    # Generative model is red, candidate blue
    tree_and_model_fill_colors <- c(
      "true_generative" = "#FF3333", # Red
      "twin_generative" = "#FFAAAA", # Light red
      "true_candidate" = "#3333FF", # Blue
      "twin_candidate" = "#AAAAFF"  # Light blue
    )

    ##### Medians for the vertical lines #####

    # Collect the medians
    # medians <- df_long %>% # nolint
    #   dplyr::group_by(tree_and_model) %>% # nolint
    #   dplyr::summarise(median = stats::median(error_value)) # nolint

    ##### Only keep 95% of x axis values #####

    index <- trunc(0.95 * length(df_long$error_value))
    x_top <- sort(df_long$error_value)[index]

    ##### Facet Labels

    expected_mbd_params <- create_paramses_mbd()
    expected_nus <- unique(expected_mbd_params$nu)
    expected_qs <- unique(expected_mbd_params$q)

    # inference models
    # need to fix this
    inference_model_labels <- c("Best", "Generative") # what you show
    names(inference_model_labels) <- c("candidate", "generative") # what's in the df

    # nu
    nu_values <- unique(df_long$nu)
    nu_labels <- paste0("nu==", nu_values) # what you show
    names(nu_labels) <- nu_values # what's in the df
    if (all(nu_values == expected_nus)) {
      df_long$nu2 <- factor(
        df_long$nu,
        labels = c("nu==1", "nu==1.5", "nu==2", "nu==2.5")
      )
    } else {
      df_long$nu2 <- factor(
        df_long$nu,
        labels = nu_labels
      )
    }

    # q
    q_values <- unique(df_long$q)
    q_labels <- paste0("q==", q_values) # what you show
    names(q_labels) <- q_values # what's in the df
    if (all(q_values == expected_qs)) {
      df_long$q2 <- factor(
        df_long$q,
        labels = c("q==0.1", "q==0.15", "q==0.2")
      )
    } else {
      df_long$q2 <- factor(
        df_long$q,
        labels = q_labels
      )
    }

    ##### Plot #####

    plots_list[[i_mu]] <- ggplot2::ggplot(
      data = df_long,
      ggplot2::aes(
        x = error_value,
        color = tree_and_model,
        fill = tree_and_model
      )
    ) +
      ggplot2::geom_density(alpha = 0.5) +
      ggplot2::facet_grid(
        nu2 + inference_model ~ q2,
        labeller =
          # ggplot2::labeller( # nolint
          #   inference_model = inference_model_labels, # nolint
          #   nu = nu_labels, # nolint
          #   q = q_labels # nolint
          # ) # nolint
          label_parsed
      ) +
      ggplot2::scale_color_manual(
        values = tree_and_model_line_colors,
        labels = get_legend_labels(df_long)
      ) +
      ggplot2::scale_fill_manual(
        values = tree_and_model_fill_colors,
        labels = get_legend_labels(df_long)
      ) +
      ggplot2::scale_x_continuous(
        minor_breaks = seq(0.0, 1.0, 0.01)
      ) +
      ggplot2::coord_cartesian(
        xlim = c(min(df_long$error_value), x_top)
      ) +
      # ggplot2::geom_vline(
      #   data = medians,
      #   ggplot2::aes(
      #     xintercept = median,
      #     color = tree_and_model
      #   ),
      #   linetype = "dashed"
      # ) +
      ggplot2::labs(
        x = "Error",
        y = "Density",
        fill = "Model and tree",
        color = "Model and tree",
        title = plot_title,
        subtitle = plot_subtitle
      ) + theme
  }

  plots_list
}
