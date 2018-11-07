#' @title Create a BD alignment from a BD tree
#' @description Create the twin BD alignment
#'   from parameters and a BD tree
#' @inheritParams default_params_doc
#' @return an alignment
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_bd_alignment <- function(
  parameters,
  bd_tree,
  mbd_tree
) {
  sequence_length <- parameters$sequence_length
  testit::assert(!is.null(sequence_length))

  # Calculate the mutation rate from the tree
  bd_mutation_rate <- raz_calc_mut_rates(
    bd_tree = bd_tree,
    mbd_tree = mbd_tree
  )$bd_mut_rate
  testit::assert(!is.null(bd_mutation_rate))
  testit::assert(is.numeric(bd_mutation_rate))

  # Root sequence is e.g. AAACCCGGGTTT
  root_sequence <- pirouette::create_blocked_dna(sequence_length)

  pirouette::sim_alignment(
    phylogeny = bd_tree,
    sequence_length = NULL,
    root_sequence = root_sequence,
    mutation_rate = bd_mutation_rate
  )
}
