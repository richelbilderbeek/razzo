#' Create figure 1
#' @inheritParams default_params_doc
#' @return Figure 1 as a ggplot2 plot
#' @author Giovanni Laudanno
#' @export
create_fig_1 <- function(
  project_folder_name
) {
  check_project_folder_name(project_folder_name)

  df0 <- collect_nltt_stats(project_folder_name)
  df00 <- tidyr::gather(
    df0,
    "i",
    "nltt",
    (1:ncol(df0))[grepl("nltt", names(df0))]
  )
  df <- df00[df00$best_or_gen == "gen" & df00$gen_model == "mbd", ]

  if (1) {
      # need to call these variables to avoid notes in check
      lambda <- df$lambda
      mu <- df$mu
      nu <- df$nu
      q <- df$q
      nltt <- df$nltt

      rm(lambda, mu, nu, q, nltt)
    }

  pl_1 <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = nu, y = nltt)
  ) +
    ggplot2::geom_point() +
    ggplot2::xlab(bquote( ~ nu ~ "")) +
    ggplot2::ggtitle("Figure 1a")
  pl_1

  pl_2 <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = q, y = nltt)
  ) +
    ggplot2::geom_point() +
    ggplot2::xlab(bquote( ~ q ~ "")) +
    ggplot2::ggtitle("Figure 1b")
  pl_2

  pl_3 <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = nu, y = nltt, colour = q)
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_color_gradientn(colours = grDevices::rainbow(5)) +
    ggplot2::ylab("nLTT") +
    ggplot2::xlab(bquote( ~ nu ~ "")) +
    ggplot2::ggtitle(bquote("nLTT error vs " ~ nu ~ ", for different values of q")) # nolint
  pl_3

  return(pl_3)
}
