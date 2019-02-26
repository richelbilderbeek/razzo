#' @title Collect nltt statistics
#' @description Collect nltt statistics
#' @inheritParams default_params_doc
#' @return a dataframe with parameters and nltt statistics
#' @author Giovanni Laudanno
#' @export
collect_nltt_stats <- function(
  project_folder_name = get_path("razzo_project")
) {
  check_project_folder_name(project_folder_name) # nolint

  # retrieve information from files
  paths <- get_data_paths(project_folder_name) # nolint internal function
  parameters <- open_parameters_file(file.path(paths[1], "parameters.RDa")) # nolint internal function
  len_nltt <- 0
  for (p in paths) {
    tab <- utils::read.csv(file.path(p, "mbd_nltt.csv"))[1, -1]
    errors <- subset(tab, select = grepl("error", names(tab)))
    len_nltt <- pmax(len_nltt, ncol(errors))
  }
  pars <- parameters$mbd_params
  par_names <- names(parameters$mbd_params)
  nltt_names <- paste0("nltt_", (1:len_nltt))
  len_pars <- length(pars)
  n_settings <- length(paths)

  # initialize dataframe components
  par_data <- data.frame(matrix(
    NA,
    ncol = len_pars,
    nrow = 2 * n_settings
  ))
  colnames(par_data) <- par_names[!grepl("model", par_names)]
  nltt_data <- data.frame(matrix(
    NA,
    ncol = len_nltt,
    nrow = 2 * n_settings
  ))
  colnames(nltt_data) <- nltt_names
  gen_model <- clock_model <- site_model <- rep("blank", 2 * n_settings)
  inference_model <- inference_model_weight <- tree_prior <- gen_model

  # collect data
  i <- 1
  for (p in paths) {
    parameters <- open_parameters_file(file.path(p, "parameters.RDa")) # nolint internal function
    nltt <- utils::read.csv(file.path(p, "mbd_nltt.csv"))[, -1]
    bd_nltt <- nltt[nltt$tree == "twin",]
    mbd_nltt <- nltt[nltt$tree == "true",]
    bd_errors <- subset(bd_nltt, select = grepl("error", names(bd_nltt)))
    mbd_errors <- subset(mbd_nltt, select = grepl("error", names(mbd_nltt)))

    # save bd results
    par_data[i, ] <- parameters$mbd_params
    nltt_data[i, ] <- unlist(unname(data.frame(
      bd_errors
    )))
    site_model[i] <- levels(droplevels(bd_nltt$site_model))
    clock_model[i] <- levels(droplevels(bd_nltt$clock_model))
    gen_model[i] <- "bd"
    inference_model[i] <- levels(droplevels(bd_nltt$inference_model))
    tree_prior[i] <- levels(droplevels(bd_nltt$tree_prior))
    if (is.na(bd_nltt$inference_model_weight)) {
      inference_model_weight[i] <- NA
    } else {
      inference_model_weight[i] <- levels(droplevels(
        bd_nltt$inference_model_weight
      ))
    }
    i <- i + 1

    # # save mbd results
    par_data[i, ] <- parameters$mbd_params
    nltt_data[i, ] <- unlist(unname(data.frame(
      mbd_errors
    )))
    site_model[i] <- levels(droplevels(mbd_nltt$site_model))
    clock_model[i] <- levels(droplevels(mbd_nltt$clock_model))
    gen_model[i] <- "mbd"
    inference_model[i] <- levels(droplevels(mbd_nltt$inference_model))
    tree_prior[i] <- levels(droplevels(mbd_nltt$tree_prior))
    if (is.na(mbd_nltt$inference_model_weight)) {
      inference_model_weight[i] <- NA
    } else {
      inference_model_weight[i] <- levels(droplevels(
        mbd_nltt$inference_model_weight
      ))
    }
    i <- i + 1
  }

  results <- cbind(
    par_data,
    gen_model,
    site_model,
    clock_model,
    inference_model,
    inference_model_weight,
    tree_prior,
    nltt_data
  )
  results
}
