#' Create a \code{twinning_params} that follows the razzo naming
#' convention and article
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_razzo_twinning_params <- function(folder_name) {
  twinning_params <- pirouette::create_twinning_params()
  twinning_params$twin_tree_filename <-
    razzo::get_tree_filename(folder_name = folder_name, tree_type = "twin")
  twinning_params$twin_alignment_filename <-
    razzo::get_alignment_filename(folder_name = folder_name, tree_type = "twin")
  twinning_params$twin_evidence_filename <-
    razzo::get_evidence_filename(folder_name = folder_name, tree_type = "twin")
  twinning_params
}
