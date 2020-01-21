#' Create a \code{twinning_params} that follows the razzo naming
#' convention and article
#' @inheritParams pirouette::create_twinning_params
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
create_razzo_twinning_params <- function(
  folder_name,
  rng_seed_twin_tree,
  rng_seed_twin_alignment
) {
  twinning_params <- pirouette::create_twinning_params(
    rng_seed_twin_tree = rng_seed_twin_tree,
    rng_seed_twin_alignment = rng_seed_twin_alignment
  )
  twinning_params$twin_tree_filename <-
    razzo::get_tree_filename(folder_name = folder_name, tree_type = "twin")
  twinning_params$twin_alignment_filename <-
    razzo::get_alignment_filename(folder_name = folder_name, tree_type = "twin")
  twinning_params$twin_evidence_filename <-
    razzo::get_evidence_filename(folder_name = folder_name, tree_type = "twin")
  twinning_params
}
