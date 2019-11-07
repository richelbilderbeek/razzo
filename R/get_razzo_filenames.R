#' Get input filename
#' @inheritParams default_params_doc
#' @export
get_input_filename <- function(
  folder_name,
  model_type
) {
  pirouette::check_model_type(model_type)
  if (model_type == "generative") {
    input_filename <- file.path(folder_name, "mbd_gen.xml")
  }
  if (model_type == "candidate") {
    input_filename <- file.path(folder_name, "mbd_best.xml")
  }
  input_filename
}

#' Get output state filename
#' @inheritParams default_params_doc
#' @export
get_output_state_filename <- function(
  folder_name,
  model_type
) {
  pirouette::check_model_type(model_type)
  if (model_type == "generative") {
    output_state_filename <- file.path(folder_name, "mbd_gen.xml.state")
  }
  if (model_type == "candidate") {
    output_state_filename <- file.path(folder_name, "mbd_best.xml.state")
  }
  output_state_filename
}

#' Get output trees filenames
#' @inheritParams default_params_doc
#' @export
get_output_trees_filenames <- function(
  folder_name,
  model_type
) {
  pirouette::check_model_type(model_type)
  if (model_type == "generative") {
    output_trees_filenames <- file.path(folder_name, "mbd_gen.trees")
  }
  if (model_type == "candidate") {
    output_trees_filenames <- file.path(folder_name, "mbd_best.trees")
  }
  output_trees_filenames
}

#' Get errors filename
#' @inheritParams default_params_doc
#' @export
get_errors_filename <- function(
  folder_name,
  model_type
) {
  pirouette::check_model_type(model_type)
  if (model_type == "generative") {
    errors_filename <- file.path(folder_name, "mbd_nltts_gen.csv")
  }
  if (model_type == "candidate") {
    errors_filename <- file.path(folder_name, "mbd_nltts_best.csv")
  }
  errors_filename
}

#' Get treelog filename
#' @inheritParams default_params_doc
#' @export
get_treelog_filename <- function(
  folder_name,
  model_type
) {
  pirouette::check_model_type(model_type)
  if (model_type == "generative") {
    treelog_filename <- file.path(folder_name, "mbd_treelog_gen.trees")
  }
  if (model_type == "candidate") {
    treelog_filename <- file.path(folder_name, "mbd_treelog_best.trees")
  }
  treelog_filename
}

#' Get tracelog filename
#' @inheritParams default_params_doc
#' @export
get_tracelog_filename <- function(
  folder_name,
  model_type
) {
  pirouette::check_model_type(model_type)
  if (model_type == "generative") {
    tracelog_filename <- file.path(folder_name, "mbd_tracelog_gen.log")
  }
  if (model_type == "candidate") {
    tracelog_filename <- file.path(folder_name, "mbd_tracelog_best.log")
  }
  tracelog_filename
}
