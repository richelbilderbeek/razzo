#' @title Create an MBD alignment from a parameters file in the same folder
#' @description Create an MBD alignment
#'   from a parameters file in the same folder
#' as that parameters file.
#' For example, for a path '/my_folder/parameters.csv', this function creates:
#' '/my_folder/mbd.fasta'.
#' @inheritParams default_params_doc
#' @return nothing
#' @author Richel J.C. Bilderbeek, Giovanni Laudanno
#' @export
raz_create_mbd_alignment <- function(
  parameters, mbd_tree
) {
  # Get the sequence length from the parameters filename
  sequence_length <- parameters$sequence_length
  testit::assert(!is.null(sequence_length))

  # Calculate the mutation rate from the tree
  # Found:
  #  * BD_mutation_rate <-  MBD_mutation_rate * (sum(MBD_tree$edge.length)/sum(BD_tree$edge.length)); # nolint
  #  * mbd_total_branch_length <- sum(mbd_tree$edge.length)
  mutation_rate <- parameters$mbd_mutation_rate
  testit::assert(!is.null(mutation_rate))

  # Root sequence is e.g. AAACCCGGGTTT
  root_sequence <- pirouette::create_blocked_dna(sequence_length)
  testit::assert(length(root_sequence) > 0)

  alignment <- pirouette::sim_alignment(
    phylogeny = mbd_tree,
    sequence_length = NA,
    root_sequence = root_sequence,
    mutation_rate = mutation_rate
  )

  alignment
}
