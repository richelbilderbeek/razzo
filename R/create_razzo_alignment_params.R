#' Create a razzo alignment parameter set that matches
#' the razzo file naming convenstions and article
#' @inheritParams default_params_doc
#' @param rng_seed RNG seed for the alignment simulation
#' @author Richel J.C. Bilderbeek
#' @export
create_razzo_alignment_params <- function(
  folder_name,
  rng_seed = 1
) {
  pirouette::create_alignment_params(
    root_sequence = pirouette::create_blocked_dna(
      length = get_razzo_dna_alignment_length() # nolint razzo function
    ),
    mutation_rate = 0.5 / get_razzo_crown_age(),
    rng_seed = rng_seed,
    fasta_filename = file.path(folder_name, "mbd.fasta")
  )
}
