#' Create a razzo alignment parameter set that matches
#' the razzo file naming convenstions and article
#' @inheritParams default_params_doc
#' @author Richel J.C. Bilderbeek
#' @export
create_razzo_alignment_params <- function(
  folder_name
) {
  pirouette::create_alignment_params(
    root_sequence = pirouette::create_blocked_dna(length = 1000),
    mutation_rate = 0.5 / get_razzo_crown_age(),
    fasta_filename = file.path(folder_name, "mbd.fasta")
  )
}
