#' Create a \code{twinning_params} that follows the razzo naming
#' convention and article
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_razzo_twinning_params <- function(folder_name) {
  twinning_params <- pirouette::create_twinning_params()
  twinning_params$twin_tree_filename <- file.path(
    folder_name, "mbd_twin.tree"
  )
  twinning_params$twin_alignment_filename <- file.path(
    folder_name, "mbd_twin.fasta"
  )
  twinning_params$twin_evidence_filename <- file.path(
    folder_name, "mbd_marg_lik_twin.csv"
  )
  twinning_params
}
