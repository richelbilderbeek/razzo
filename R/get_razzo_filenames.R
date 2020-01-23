#' Get output log filename
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
get_output_log_filename <- function(
  folder_name,
  model_type
) {
  pirouette::check_model_type(model_type)
  if (model_type == "generative") {
    output_log_filename <- file.path(folder_name, "mbd_gen.log")
  }
  if (model_type == "candidate") {
    output_log_filename <- file.path(folder_name, "mbd_best.log")
  }
  output_log_filename
}

#' Get output trees filenames
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
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

#' Get input filename
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
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
#' @author Giovanni Laudanno
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

#' Get errors filename
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
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
#' @author Giovanni Laudanno
#' @export
get_treelog_filename <- function(
  folder_name,
  model_type
) {
  pirouette::check_model_type(model_type)
  if (model_type == "generative") {
    treelog_filename <- file.path(folder_name, "mbd_gen.trees")
  }
  if (model_type == "candidate") {
    treelog_filename <- file.path(folder_name, "mbd_best.trees")
  }
  treelog_filename
}

#' Get tracelog filename
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
get_tracelog_filename <- function(
  folder_name,
  model_type
) {
  pirouette::check_model_type(model_type)
  if (model_type == "generative") {
    tracelog_filename <- file.path(folder_name, "mbd_gen.log")
  }
  if (model_type == "candidate") {
    tracelog_filename <- file.path(folder_name, "mbd_best.log")
  }
  tracelog_filename
}

#' Get evidence filename
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
get_evidence_filename <- function(
  folder_name,
  tree_type
) {
  pirouette::check_tree_type(tree_type)
  if (tree_type == "true") {
    evidence_filename <- file.path(folder_name, "mbd_marg_lik.csv")
  }
  if (tree_type == "twin") {
    evidence_filename <- file.path(folder_name, "mbd_marg_lik_twin.csv")
  }
  evidence_filename
}

#' Get alignment filename
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
get_alignment_filename <- function(
  folder_name,
  tree_type
) {
  pirouette::check_tree_type(tree_type)
  if (tree_type == "true") {
    fasta_filename <- file.path(folder_name, "mbd.fasta")
  }
  if (tree_type == "twin") {
    fasta_filename <- file.path(folder_name, "mbd_twin.fasta")
  }
  fasta_filename
}

#' Get tree filename
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno, Richel J.C. Bilderbeek
#' @examples
#' library(testthat)
#'
#' expect_equal(
#'   get_tree_filename(folder_name = "", tree_type = "true"),
#'   "/mbd.tree"
#' )
#' expect_equal(
#'   get_tree_filename(folder_name = "", tree_type = "twin"),
#'   "/mbd_twin.tree"
#' )
#' @export
get_tree_filename <- function(
  folder_name,
  tree_type
) {
  pirouette::check_tree_type(tree_type)
  if (tree_type == "true") {
    tree_filename <- file.path(folder_name, "mbd.tree")
  }
  if (tree_type == "twin") {
    tree_filename <- file.path(folder_name, "mbd_twin.tree")
  }
  tree_filename
}

#' Get seed folder name
#' @inheritParams default_params_doc
#' @author Giovanni Laudanno
#' @export
get_seed_folder_name <- function(
  project_folder_name,
  mbd_params
) {
  data_folder_name <- "data"
  parsettings_name <- paste0(
    mbd_params$lambda,
    "-",
    mbd_params$mu,
    "-",
    mbd_params$nu,
    "-",
    mbd_params$q
  )
  seed <- mbd_params$seed
  seed_folder_name <- file.path(
    project_folder_name,
    data_folder_name,
    parsettings_name,
    seed
  )
  seed_folder_name
}
