#' Check if \code{project_folder_name} ends with \code{razzo_project}
#' and is the name of an existing folder.
#' Will throw if not.
#' Else will do nothing.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richèl J.C. Bilderbeek
#' @export
check_project_folder_name <- function(project_folder_name) {
  if (
    is.na(
      stringr::str_match(
        string = project_folder_name,
        pattern =
          "(razzo_project|raket_werper)(_........)?(_regen_data|new_logs)?"
      )[1, 1]
    )
  ) {
    stop(
      "'project_folder_name' must end with 'razzo_project' or 'raket_werper'"
    )
  }
  if (!dir.exists(project_folder_name)) {
    stop(
      "'project_folder_name' absent. ",
      "Folder with path '", project_folder_name, "' not found"
    )
  }
}

#' Opens a parameter file and parses it
#' @inheritParams default_params_doc
#' @return the razzo parameters
#' @author Richèl J.C. Bilderbeek, Giovanni Laudanno
#' @export
open_parameters_file <- function(
  parameters_filename
) {
  beautier::check_file_exists(parameters_filename)

  razzo_params <- NULL
  if (
    tools::file_ext(parameters_filename) == "Rda" ||
    tools::file_ext(parameters_filename) == "RDa"
  ) {
    razzo_params <- readRDS(parameters_filename)
    razzo::check_razzo_params(razzo_params)
  } else {
    # Remove the first column, as it is an unused row name
    razzo_params <- utils::read.csv(parameters_filename)[, -1]

    testit::assert(razzo_params$mbd_params$lambda >= 0)
    testit::assert(razzo_params$mbd_params$mu >= 0)
    testit::assert(razzo_params$mbd_params$nu >= 0)
    testit::assert(razzo_params$mbd_params$q >= 0)
    testit::assert(razzo_params$mbd_params$q <= 1)
  }
  razzo_params
}

#' Get the names of the supported generative models
#' @return the generative models
#' @author Giovanni Laudanno, Richèl J.C. Bilderbeek
#' @examples
#' get_gen_models()
#' @export
get_gen_models <- function() {
  c("bd", "mbd")
}

#' Retrieve the best candidates from the data
#' @inheritParams default_params_doc
#' @return list of models
#' @author Giovanni Laudanno
#' @export
get_best_model <- function(seed_folder) {
  p <- seed_folder
  data_filename <- "mbd_marg_lik.csv"
  twin_data_filename <- pirouette::to_twin_filename(data_filename)
  bd_data <- utils::read.csv(file.path(p, twin_data_filename))[-1]
  mbd_data <- utils::read.csv(file.path(p, data_filename))[-1]
  bd_site_model <- bd_data[
    bd_data$weight == max(bd_data$weight), "site_model_name"]
  bd_clock_model <- bd_data[
    bd_data$weight == max(bd_data$weight), "clock_model_name"]
  bd_tree_prior <- bd_data[
    bd_data$weight == max(bd_data$weight), "tree_prior_name"]
  mbd_site_model <- mbd_data[
    mbd_data$weight == max(mbd_data$weight), "site_model_name"]
  mbd_clock_model <- mbd_data[
    mbd_data$weight == max(mbd_data$weight), "clock_model_name"]
  mbd_tree_prior <- mbd_data[
    mbd_data$weight == max(mbd_data$weight), "tree_prior_name"]
  list(
    twin = list(
      site_model = levels(droplevels(bd_site_model)),
      clock_model = levels(droplevels(bd_clock_model)),
      tree_prior = levels(droplevels(bd_tree_prior))
    ),
    true = list(
      site_model = levels(droplevels(mbd_site_model)),
      clock_model = levels(droplevels(mbd_clock_model)),
      tree_prior = levels(droplevels(mbd_tree_prior))
    )
  )
}

#' Retrieve the generative model from the data
#' @inheritParams default_params_doc
#' @return list of models
#' @author Giovanni Laudanno
#' @export
get_generative_model <- function(seed_folder) {
  p <- seed_folder
  data_filename <- "mbd_marg_lik.csv"
  twin_data_filename <- pirouette::to_twin_filename(data_filename)
  bd_data <- utils::read.csv(file.path(p, twin_data_filename))[-1]
  mbd_data <- utils::read.csv(file.path(p, data_filename))[-1]
  bd_site_model <- bd_data[1, "site_model_name"]
  bd_clock_model <- bd_data[1, "clock_model_name"]
  bd_tree_prior <- bd_data[1, "tree_prior_name"]
  mbd_site_model <- mbd_data[1, "site_model_name"]
  mbd_clock_model <- mbd_data[1, "clock_model_name"]
  mbd_tree_prior <- mbd_data[1, "tree_prior_name"]
  list(
    twin = list(
      site_model = levels(droplevels(bd_site_model)),
      clock_model = levels(droplevels(bd_clock_model)),
      tree_prior = levels(droplevels(bd_tree_prior))
    ),
    true = list(
      site_model = levels(droplevels(mbd_site_model)),
      clock_model = levels(droplevels(mbd_clock_model)),
      tree_prior = levels(droplevels(mbd_tree_prior))
    )
  )
}
